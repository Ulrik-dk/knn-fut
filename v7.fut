open import "lib/github.com/diku-dk/sorts/merge_sort"
open import "lib/github.com/diku-dk/sorts/quick_sort"
open import "lib/github.com/diku-dk/sorts/radix_sort"
open import "lib/batch-merge-sort"
open import "bf"
open import "constants"
open import "kd-tree-common"

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

let traverse_once [tree_size][tree_size_plus][d]
                           (h: i32)
                           (q: [d]f32)
                           (stack: i32)
                           (leaf_index: i32)
                           (wnnd: f32)
                           (tree_dims: [tree_size]i32)
                           (tree_meds: [tree_size]f32)
                           (lbs: [tree_size_plus][d]f32)
                           (ubs: [tree_size_plus][d]f32) =

  let getPackedInd (stk: i32) (level: i32) : bool =
    i32.get_bit level stk |> (>0)
  let setPackedInd (stk: i32) (level: i32) (v: i32) : i32 =
    i32.set_bit level stk v

  let tree_index = leaf_index + tree_size

  let (parent_index, stack, rec_node, _) =
  loop (node_index, stack, rec_node, level) =
       (tree_index, stack, -1, h)
  while (node_index != 0) && (rec_node < 0) do
    let parent_index = getParent node_index
    let sibling_index = getSibling node_index in
    if getPackedInd stack level
    then -- sibling (second node) already visited, go up the tree
      (parent_index, setPackedInd stack level 0, rec_node, level-1)
    else
      --- Test if this or both
      if (special_test q wnnd lbs[sibling_index] ubs[sibling_index])
        --((f32.abs (q[tree_dims[parent_index]] - tree_meds[parent_index])) < wnnd)
      then (parent_index, setPackedInd stack level 1, sibling_index, level)
      else (parent_index, setPackedInd stack level 0, rec_node, level-1)

  let new_leaf =
    if rec_node == -1 && parent_index == 0
      then -1 -- we are done, we are at the root node and its second child has been visited
      else find_natural_leaf rec_node q tree_dims tree_meds
  in (new_leaf, stack)

let v7 [n][m][d] (leaf_size_lb: i32) (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =

    -- pad the array of points
    let pad_elm = replicate d f32.inf
    let (padded_P, leaf_size) = pad P pad_elm leaf_size_lb

    -- get the height and build the balanced tree
    -- original_P_inds is used to put correct the knns indices in the end
    let height = get_height leaf_size (length padded_P)
    let (tree_dims, tree_meds, global_lbs, global_ubs, leaf_structure, original_P_inds) = build_balanced_tree padded_P height leaf_size

    let size_promise = length tree_dims
    let tree_dims = tree_dims :> [size_promise]i32
    let tree_meds = tree_meds :> [size_promise]f32

    let size_promise = length global_lbs
    let global_lbs = global_lbs :> [size_promise][d]f32
    let global_ubs = global_ubs :> [size_promise][d]f32

    let leaf_indices = map (\q -> find_natural_leaf 0 q tree_dims tree_meds) Q

    -- initialize the loop variables
    let stacks = replicate m 0i32
    let knns = unflatten m k <| zip (replicate (k*m) (-1)) (replicate (k*m) f32.inf)
    let ordered_all_knns = copy knns
    let Q_inds = iota m

    let res = -- main loop
    loop (ordered_all_knns, knns, leaf_indices, stacks, Q, Q_inds) while (length leaf_indices > 0) do

      -- a. brute-force on previous leaves and ongoing queries
      let knns = map3 (\ q knn leaf_index ->
                    bruteForce q knn leaf_structure[leaf_index] leaf_index
                  ) Q knns leaf_indices

      -- b. traverse-once to find the next leaves
      let (leaf_indices, stacks) = unzip <| map4 (\ q stack leaf_index wnnd ->
                                            traverse_once height q stack leaf_index wnnd tree_dims tree_meds global_lbs global_ubs
                                          ) Q stacks leaf_indices (map get_wnnd knns)

      -- c. partition so that the queries that finished come last
      let (done_inds, cont_inds) = partition (\i -> leaf_indices[i] == -1) (iota <| length leaf_indices)

      -- d. update the ordered_all_knns for the queries that have finished
      let ordered_all_knns = scatter2D ordered_all_knns
                                (map (\i -> Q_inds[i]) done_inds)
                                (map (\i ->   knns[i]) done_inds)

      -- e. keep only the ongoing parts of the partitioned arrays: Q, knns, leaf_indices, stacks
      -- we want to recover cont_inds of knns, leaf_indices, stacks, Q_inds, Q
      -- but we want to place them, not in the order that cont_inds are in now,
      -- but in the order they will be in when we sort them according to leaves

      -- 1. gather leaf_indices
      let num_active = length cont_inds

      let leaf_indices = gather1d cont_inds leaf_indices :> [num_active]i32

      -- 3. finally, gather using this reordered cont_inds array
      let stacks = gather1d cont_inds stacks
      let Q_inds = gather1d cont_inds Q_inds
      let knns = gather2d cont_inds knns
      let Q = gather2d cont_inds Q

      -- finish iteration
      in (ordered_all_knns, knns, leaf_indices, stacks, Q, Q_inds)

    -- change the knns p-indices so they point to their original indices and not the sorted ones
    let finished_all_knns = unzip_matrix <| (.0) <| res
    let fixed_knn_inds = gather1d (flatten finished_all_knns.0) original_P_inds |> unflatten m k
    in (fixed_knn_inds, finished_all_knns.1)

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v7 GetLeafSizeLb GetK P Q |> (.1)

-- ==
-- input @ data/test1.in
-- output @ data/test1.out
-- input @ data/test2.in
-- output @ data/test2.out
