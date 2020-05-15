open import "util"
open import "batch-merge-sort"
open import "constants"
---- for functions that are used often and specific for the knn algorithm

let pad [n][d] (P: [n][d]f32) (leaf_size_lb: i32) : ([][]f32, i32, i32, i32, i32, i32) =
    let num_default_leaves = n / leaf_size_lb
    let num_leaves = (2**) <| i32_log2 num_default_leaves
    let num_redist = n - (num_leaves * leaf_size_lb)
    let num_excess = num_redist % num_leaves
    let num_padding = if num_excess == 0 then 0 else (num_leaves - num_excess)
    let leaf_size = leaf_size_lb + (num_redist / num_leaves) + (if num_excess > 0 then 1 else 0)
    let h = (i32_log2 num_leaves) - 1
    let num_tree_nodes = num_leaves - 1
    let num_bounds = num_tree_nodes + num_leaves
    let pad_elm = replicate d GetPadValue
    in (P ++ (replicate num_padding pad_elm), leaf_size, num_leaves, h, num_tree_nodes, num_bounds)

-- P: set of n d-dimensional points with padding
-- h: height of the tree to be constructed
let build_balanced_tree [n][d] (P: [n][d]f32)
                               (num_leaves: i32)
                               (num_tree_nodes: i32)
                               (num_bounds: i32)
                               (h: i32)
                               (leaf_size: i32)
                               :
                               ([num_tree_nodes]i32,
                                [num_tree_nodes]f32,
                                [num_bounds][d]f32,
                                [num_bounds][d]f32,
                                [num_leaves][leaf_size][d]f32,
                                []i32) =
    let Pinds = iota n
    let tree_dims = replicate num_tree_nodes 0i32
    let tree_meds = replicate num_tree_nodes 0.0f32
    let global_lbs = replicate (num_bounds*d) 0.0f32 |> unflatten num_bounds d
    let global_ubs = replicate (num_bounds*d) 0.0f32 |> unflatten num_bounds d

    -- building the tree and sorting the inds of the points in a loop,
    -- one loop for every level of the tree
    let (tree_dims, tree_meds, global_lbs, global_ubs, Pinds) =
    loop (tree_dims, tree_meds, global_lbs, global_ubs, Pinds) for depth < (h+1) do
      -- each segment corresponds to the set of points split by a
      -- given node at the current level, and we look at that segment
      -- in order to create the node
      let segment_len = n >> depth
      let segment_count = n / segment_len

      -- unflattening the point inds st. each node has easy access
      let segment_Pinds = unflatten segment_count segment_len Pinds

      -- mapping over iota over the number of nodes in the current level
      -- creates the indices into the tree for each new dim-median pair
      -- the dim-median pairs themselves
      -- and the new ordering of the indices for the points

      -- the points in this segment
      let my_segments = map (\sgm_inds ->
                            map (\ind ->
                                  map (\j -> P[ind, j]) (iota d)
                                ) sgm_inds
                        ) segment_Pinds

      -- for every segment, the dimension chosen, and the upper and lower bounds for that dimension
      let (dim_inds, lbs, ubs) =
                  unzip3 <|
                  map (\i ->
                    let my_segment_T = transpose my_segments[i] |> intrinsics.opaque
                    let mins = map (\row -> reduce_comm f32.min f32.highest row) my_segment_T |> intrinsics.opaque
                    let maxs = map (\row -> reduce_comm f32.max f32.lowest row) my_segment_T |> intrinsics.opaque
                    let difs = map2(-) maxs mins |> intrinsics.opaque
                    let (_, dim_ind) = reduce_comm (\(dif1, i1) (dif2, i2) ->
                      if(dif1 > dif2)
                        then (dif1, i1)
                        else (dif2, i2)
                      ) (f32.lowest, -1i32) (zip difs (iota d))
                    in (dim_ind, mins, maxs)
                  ) <| iota segment_count

      let values_indices = map (\i -> zip (map(\p -> p[dim_inds[i]]) my_segments[i]) segment_Pinds[i] ) <| iota segment_count

      -- the index is only a passenger, so the value we put in the neutral element does not matter
      -- COSMIN: here (f32.highest, segment_len) are the elements by which mergeSorts
      --         pads to a power of two: you need to make sure they are ordered at the end!
      let (sorted_values, sorted_Pinds) = unzip_matrix <| batch_merge_sort (f32.highest, segment_len)
                                                                (\(a,i1) (b,i2) ->
                                                                  if a < b then true  else
                                                                  if a > b then false else
                                                                  i1 <= i2
                                                                ) values_indices

      let (t_inds, dims_medians) = unzip <| map (\i ->
          -- median value picked from sorted values
          let median = sorted_values[i,(segment_len-1)/2]

          -- index to place this median-value/dim-ind pair into the tree
          let t_ind = i + segment_count - 1
          in (t_ind, median)
        ) <| iota segment_count

      -- putting the new tree-medians and dimensions onto the tree
      let tree_dims  = scatter tree_dims  t_inds dim_inds
      let tree_meds  = scatter tree_meds  t_inds dims_medians
      let global_lbs = scatter global_lbs t_inds lbs
      let global_ubs = scatter global_ubs t_inds ubs

      -- continuing the loop
      in (tree_dims, tree_meds, global_lbs, global_ubs, (flatten sorted_Pinds))

    -- "sorts" the points P such that they now align with the new ordering of the inds
    let reordered_P = gather2d Pinds P
    let original_P_inds = gather1d Pinds (iota n)

    -- returns the tree and the reordered points, and their relation to their original indices
    let leaf_structure = unflatten_3d num_leaves leaf_size d (flatten reordered_P)

    let (leaf_structure_lbs, leaf_structure_ubs) =
                unzip <|
                map (\i ->
                  let my_segment_T = transpose leaf_structure[i] |> intrinsics.opaque
                  let mins = map (\row -> reduce_comm f32.min f32.highest row) my_segment_T |> intrinsics.opaque
                  let maxs = map (\row -> reduce_comm f32.max f32.lowest row) my_segment_T |> intrinsics.opaque
                  in (mins, maxs)
                ) <| indices leaf_structure

    let inds_of_leaf_bounds = map (+num_tree_nodes) <| iota num_leaves
    let global_lbs = scatter global_lbs inds_of_leaf_bounds leaf_structure_lbs
    let global_ubs = scatter global_ubs inds_of_leaf_bounds leaf_structure_ubs

    in (tree_dims, tree_meds, global_lbs, global_ubs, leaf_structure, original_P_inds)

----------- traversal ----------
let getParent (node_index: i32) = (node_index-1) / 2
let getLeftChild (node_index: i32) = (node_index * 2) + 1
let getRightChild (node_index: i32) = (node_index * 2) + 2
let getSibling (node_index: i32) = if (node_index % 2 == 0) then node_index-1 else node_index+1
let isLeaf (h: i32) (node_index: i32) = node_index >= (1 << (h+1)) - 1
let getQuerriedLeaf (h: i32) (ppl: i32) (q: f32) =
    let leaf_ind = (t32 q) / ppl
    in  leaf_ind + (1<<(h+1)) - 1

-- i is the starting-index into the tree.
let find_natural_leaf [tree_size][d]
                      (i: i32)
                      (q: [d]f32)
                      (tree_dims: [tree_size]i32)
                      (tree_meds: [tree_size]f32) : i32 =
    let i = loop i while (i < tree_size) do
      if (q[tree_dims[i]] > tree_meds[i])
        then getRightChild(i)
        else getLeftChild(i)
    in i - tree_size --translate tree-index to leaf-structure-index

let getPackedInd (stk: i32) (level: i32) : bool =
  i32.get_bit level stk |> (>0)
let setPackedInd (stk: i32) (level: i32) (v: i32) : i32 =
  i32.set_bit level stk v

-- worst nearest neighbor distance
let get_wnnd [k] (knn: [k](i32, f32)) : f32 =
    knn[k-1].1

let traverse_once [tree_size][d]
                           (h: i32)
                           (q: [d]f32)
                           (stack: i32)
                           (leaf_index: i32)
                           (wnnd: f32)
                           (tree_dims: [tree_size]i32)
                           (tree_meds: [tree_size]f32) =
  let tree_index = leaf_index + tree_size
  let (_, stack, rec_node, _) =
  loop (node_index, stack, rec_node, level) =
       (tree_index, stack, -1, h)
  while (node_index != 0) && (rec_node == -1) do
    let parent_index = getParent node_index in
    if getPackedInd stack level
      || ((f32.abs (q[tree_dims[parent_index]] - tree_meds[parent_index])) >= wnnd)
    then (parent_index, setPackedInd stack level 0, rec_node, level-1)
    else (parent_index, setPackedInd stack level 1, getSibling node_index, level)

  let new_leaf =
    if rec_node == -1
      then -1 -- we are done, we are at the root node and its second child has been visited
      else find_natural_leaf rec_node q tree_dims tree_meds
  in (new_leaf, stack)

let traverse_once_mk2 [tree_size]
                      [tree_size_plus]
                      [d]
                       (h: i32)
                       (q: [d]f32)
                       (stack: i32)
                       (leaf_index: i32)
                       (wnnd: f32)
                       (done_indicator: i32)
                       (tree_dims: [tree_size]i32)
                       (tree_meds: [tree_size]f32)
                       (lbs: [tree_size_plus][d]f32)
                       (ubs: [tree_size_plus][d]f32) =
 let special_test [d] (q: [d]f32)
                      (wnnd: f32)
                      (lb: [d]f32)
                      (ub: [d]f32) : bool =
     let acc = loop acc = 0f32 for j < d do
       let var = q[j]
       let u = ub[j]
       let l = lb[j]
       let dif = if var > u
                 then u - var
                 else if var < l
                      then var - l
                      else 0.0f32
       in acc + (dif*dif)
     in (f32.sqrt acc) < wnnd

  let tree_index = leaf_index + tree_size
  let (_, stack, rec_node, _) =
  loop (node_index, stack, rec_node, level) =
       (tree_index, stack, -1, h)
  while (node_index != 0) && (rec_node == -1) do
    let parent_index = getParent node_index
    let sibling_index = getSibling node_index in
    if getPackedInd stack level
      || !(special_test q wnnd lbs[sibling_index] ubs[sibling_index])
    then (parent_index, setPackedInd stack level 0, rec_node, level-1)
    else (parent_index, setPackedInd stack level 1, sibling_index, level)

  let new_leaf =
    if rec_node == -1
      then done_indicator -- we are done, we are at the root node and its second child has been visited
      else find_natural_leaf rec_node q tree_dims tree_meds
  in (new_leaf, stack)
