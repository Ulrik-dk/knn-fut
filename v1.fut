open import "lib/github.com/diku-dk/sorts/merge_sort"
open import "lib/github.com/diku-dk/sorts/radix_sort"
open import "lib/batch-merge-sort"
open import "util"
open import "bf"

let my_maxf32 (a: f32) (b: f32) =
    if f32.isinf a then b
    else if f32.isinf b then a
    else f32.max a b

-- size of leafs will be in [leaf_size_lb ... (leaf_size_lb*2)-1]
-- guarantees num_pad_elms < num_leaves
let round_down_to_pow_2 (x: i32) : i32 =
  (2**) <| i32_log2 x

let h_from_l_sz (l_sz: i32) (n: i32) : i32 =
  let num_leaves = n / l_sz
  let h = i32_log2 num_leaves
  in h-1

let pad 't [n] (P: [n]t) (pad_elm: t) (leaf_size_lb: i32) : ([]t, i32) =
    let num_default_leaves = n / leaf_size_lb
    let num_leaves = round_down_to_pow_2 num_default_leaves
    let num_redist = n - (num_leaves * leaf_size_lb)
    let num_excess = num_redist % num_leaves
    let num_padding = if num_excess == 0 then 0 else (num_leaves - num_excess)
    let l_sz = leaf_size_lb + (num_redist / num_leaves) + (if num_excess > 0 then 1 else 0)
    in (P ++ (replicate num_padding pad_elm), l_sz)

-- P: set of n d-dimensional points with padding
-- h: height of the tree to be constructed
let build_balanced_tree [n][d] (P: [n][d]f32) (h: i32) : ([](i32, f32, f32, f32), [][]f32, []i32) =
    -- the number of leaves is determined from the height
    let num_leaves = 1<<(h+1)
    -- the indices of the points
    let Pinds = iota n
    -- the number of nodes in the tree from the number of leaves
    let num_tree_nodes = num_leaves - 1

    -- the tree itself, empty to begin with
    let tree = zip4 (replicate num_tree_nodes 0i32) (replicate num_tree_nodes 0.0f32) (replicate num_tree_nodes 0.0f32) (replicate num_tree_nodes 0.0f32)

    -- building the tree and sorting the inds of the points in a loop,
    -- one loop for every level of the tree
    let (tree, Pinds) = loop (tree, Pinds) for depth < (h+1) do
      -- each segment corresponds to the set of points split by a
      -- given node at the current level, and we look at that segment
      -- in order to create the node

      -- the length of the segment in each node at the current level
      let seg_len = n >> depth

      -- the number of nodes at the current level in the tree
      let seg_cnt = n / seg_len

      -- unflattening the point inds st. each node has easy access
      let seg_Pinds = unflatten seg_cnt seg_len Pinds

      -- mapping over iota over the number of nodes in the current level
      -- creates the indices into the tree for each new dim-median pair
      -- the dim-median pairs themselves
      -- and the new ordering of the indices for the points

      -- the points in this segment
      let my_segs = map (\sgm_inds ->
                            map (\ind ->
                                    map (\j -> P[ind, j]) (iota d)
                                ) sgm_inds
                        ) seg_Pinds

      -- for every segment, the dimension chosen, and the upper and lower bounds for that dimension
      let (dim_inds, ubs, lbs) =
                  unzip3 <|
                  map (\i ->
                    let my_seg_T = transpose my_segs[i] |> intrinsics.opaque
                    let mins = map (\row -> reduce_comm f32.min f32.highest row) my_seg_T |> intrinsics.opaque
                    let maxs = map (\row -> reduce_comm my_maxf32 f32.lowest row) my_seg_T |> intrinsics.opaque
                    let difs = map2(-) mins maxs |> intrinsics.opaque
                    let (_, dim_ind) = reduce_comm (\(dif1, i1) (dif2, i2) ->
                      if(dif1 > dif2)
                        then (dif1, i1)
                        else (dif2, i2)
                      ) (f32.lowest, -1i32) (zip difs (iota d))
                    in (dim_ind, maxs[dim_ind], mins[dim_ind])
                  ) <| iota seg_cnt

      let valss_indss = map (\i -> zip (map(\p -> p[dim_inds[i]]) my_segs[i]) seg_Pinds[i] ) <| iota seg_cnt

      -- the index is only a passenger, so the value we put in the neutral element does not matter
      -- COSMIN: here (f32.highest, seg_len) are the elements by which mergeSorts
      --         pads to a power of two: you need to make sure they are ordered at the end!
      let (s_valss, sPinds) = unzip_matrix <| batch_merge_sort (f32.highest, seg_len)
                                                                (\(a,i1) (b,i2) ->
                                                                  if a < b then true  else
                                                                  if a > b then false else
                                                                  i1 <= i2
                                                                ) valss_indss

      let (t_inds, dims_medians) = unzip <| map (\i ->
          -- median value picked from sorted values
          let median = s_valss[i,(seg_len-1)/2]

          -- index to place this median-value/dim-ind pair into the tree
          let t_ind = i + seg_cnt - 1
          in (t_ind, median)
        ) <| iota seg_cnt

      -- putting the new tree-medians and dimensions onto the tree
      let tree = scatter tree t_inds (zip4 dim_inds dims_medians ubs lbs)

      -- continuing the loop
      in (tree, (flatten sPinds))

    -- "sorts" the points P such that they now align with the new ordering of the inds
    let reordered_P = gather2d (Pinds :> [n]i32) P

    -- TODO: Is this the correct way to show where a given P in its new location came from?
    let original_P_inds = gather1d (Pinds :> [n]i32) (iota n)

    -- returns the tree and the reordered points, and their relation to their original indices
    in (tree, reordered_P, original_P_inds)

----------- traversal ----------

let getParent (node_index: i32) = (node_index-1) / 2
let getLeftChild (node_index: i32) = (node_index * 2) + 1
let getRightChild (node_index: i32) = (node_index * 2) + 2
let getSibling (node_index: i32) = if (node_index%2==0) then node_index-1 else node_index+1
let isLeaf (h: i32) (node_index: i32) = node_index >= (1 << (h+1)) - 1
let getQuerriedLeaf (h: i32) (ppl: i32) (q: f32) =
    let leaf_ind = (t32 q) / ppl
    in  leaf_ind + (1<<(h+1)) - 1

-- TODO: clean this up
let find_natural_leaf [d][tsz] (q: [d]f32) (tree_dims: [tsz]i32) (tree_meds: [tsz]f32) : i32 =
    let i = 0
    let i = loop i while (i < tsz) do
      if (q[tree_dims[i]] <= tree_meds[i])
        then getLeftChild(i)
        else getRightChild(i)
    in i - tsz

-- for one query - should be used in a map
-- TODO: clean this up
let traverse_once [tsz][d] (h: i32)
                           (q: [d]f32)
                           (stack: i32)
                           (lidx: i32) -- last leaf index
                           (wnnd: f32) -- worst nearest neighbor distance
                           (tree: [tsz](i32, f32, f32, f32)) =
  let getPackedInd (stk: i32) (level: i32) : bool =
    i32.get_bit level stk |> (>0)
  let setPackedInd (stk: i32) (level: i32) (v: bool) : i32 =
    i32.set_bit level stk (if v then 1 else 0)

  let (tree_dims, tree_meds, _, _) = unzip4 tree
  let (parent_rec, stack, _, rec_node) =
    loop (node_index, stack, level, rec_node) =
         (lidx,       stack, h,     -1)
          while (node_index != 0) && (rec_node < 0) do
              let parent_index = getParent node_index
              let sibling_index = getSibling node_index in
              --TODO: something here? I forgot
              --  if (!(getPackedInd stack level) && f32.abs(q[tree_dims[parent_index]] - tree_meds[parent_index]) < wnnd)
              --  then (parent_index, setPackedInd stack level true, level, sibling_index)
              --  else (parent_index, setPackedInd stack level false, level-1, rec_node)
              if getPackedInd stack level
              then -- sibling (second node) already visited, go up the tree
                   (parent_index, setPackedInd stack level false, level-1, rec_node)
              else let to_visit = f32.abs(q[tree_dims[parent_index]] - tree_meds[parent_index]) < wnnd
                   in if !to_visit
                      then (parent_index, setPackedInd stack level false, level-1, rec_node)
                      else (parent_index, setPackedInd stack level true, level, sibling_index)


  let new_leaf =
    if parent_rec == 0 && rec_node == -1
    then -1 -- we are done, we are at the root node and its second child has been visited
    else loop node_index = rec_node
      while !(isLeaf h node_index) do
        if q[tree_dims[node_index]] < tree_meds[node_index]
          then getLeftChild node_index -- if q less than median, go left
          else getRightChild node_index -- if q greater than median, go right
  in (new_leaf, stack)

-- ==
-- entry: main
--
-- compiled random input { 256i32 [8388608][8]f32 }
-- compiled random input { 256i32 [2097152][16]f32 }
-- compiled random input { 256i32 [8388608][16]f32 }
-- compiled random input { 256i32 [1048576][16]f32 }

-- execute with:
-- $ futhark dataget v1.fut "256i32 [1048576][16]f32" | ./v1 -t /dev/stderr > /dev/null
-- for profiling:
-- $ futhark dataget v1.fut "256i32 [1048576][16]f32" | ./v1 -P -t /dev/stderr > /dev/null

-- TODO: reconsider this function
let get_wnnd [k] (knns: [k](i32, f32)) : f32 = knns[k-1].1

-- TODO: clean this up
let v1 [n][m][d] (leaf_size_lb: i32) (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =
    let pad_elm = replicate d f32.inf
    -- pad and shadow out old P and n
    let (P, leaf_size) = pad P pad_elm leaf_size_lb
    let n = length P

    let h = h_from_l_sz leaf_size n
    let (tree, P, original_P_inds) = build_balanced_tree P h
    -- let num_leaves = (length tree) + 1
    -- TODO: construct upper and lower-bounds for each segment of P, ie. for the leaves
    let (tree_dims, tree_meds, _, _) = unzip4 tree

    -- leaf indices
    let lidxs = map (\q -> find_natural_leaf q tree_dims tree_meds) Q

    let stacks = replicate m 0i32
    -- let visited = replicate num_leaves 0i32

    let Q_inds = iota m
    let knns = unflatten m k <| zip (replicate (k*m) (-1)) (replicate (k*m) f32.inf)
    let ordered_all_knns = copy knns
    in unzip_matrix <| (.0) <|
    loop (ordered_all_knns, knns, Q_inds, lidxs, stacks) while (length lidxs > 0) do

      -- 1. brute-force on previous leaves and ongoing queries
      let knns = map3 (\ q_ind knn lidx ->
                    let ref_inds = iota leaf_size |> map (+(lidx*leaf_size))
                    let ref_o_inds = gather1d ref_inds original_P_inds
                    in bruteForce Q[q_ind] knn P ref_inds ref_o_inds
                  ) Q_inds knns lidxs

      -- 2. traverse-once to find the next leaves
      let (lidxs, stacks) = unzip <| map4 (\ q_ind stack lidx wnnd -> traverse_once h Q[q_ind] stack lidx wnnd tree) Q_inds stacks lidxs (map get_wnnd knns)

      -- 3. partition so that the queries that finished come last
      let (done_inds, cont_inds) = partition (\i -> lidxs[i] == -1) (iota <| length Q_inds)

      -- 4. update the ordered_all_knns for the queries that have finished
      let ordered_all_knns = scatter ordered_all_knns
                                (map (\i -> Q_inds[i]) done_inds)
                                (map (\i ->   knns[i]) done_inds)

      -- 5. keep only the ongoing parts of the partitioned arrays: Q, knns, lidxs, stacks
      let new_len = length cont_inds
      let knns = gather2d cont_inds knns :> [new_len][k](i32, f32)
      let lidxs = gather1d cont_inds lidxs
      let stacks = gather1d cont_inds stacks
      let Q_inds = gather1d cont_inds Q_inds

      in (ordered_all_knns, knns, Q_inds, lidxs, stacks)

entry main [n][m][d] (leaf_size_lb: i32) (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =
  v1 leaf_size_lb k P Q

entry validation [n][m][d] (leaf_size_lb: i32) (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =
  let (a, b) = simpleBruteForce k P Q
  let (c, d) = v1 leaf_size_lb k P Q
  in (a, c, b, d)

--entry test [n][d] (P: [n][d]f32) =
--  main 256i32 P
