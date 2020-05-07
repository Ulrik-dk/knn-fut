open import "lib/batch-merge-sort"
open import "bf"
open import "constants"
open import "kd-tree-common"

let v1 [n][m][d] (leaf_size_lb: i32) (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =

    -- pad the array of points
    let pad_elm = replicate d GetPadValue
    let (padded_P, leaf_size) = pad P pad_elm leaf_size_lb

    -- get the height and build the balanced tree
    -- original_P_inds is used to put correct the knns indices in the end
    let height = get_height leaf_size (length padded_P)
    let (tree_dims, tree_meds, _, _, leaf_structure, original_P_inds) = build_balanced_tree padded_P height leaf_size

    let size_promise = length tree_dims
    let tree_dims = tree_dims :> [size_promise]i32
    let tree_meds = tree_meds :> [size_promise]f32

    -- find the initial leaves of the queries
    let leaf_indices = map (\q -> find_natural_leaf 0 q tree_dims tree_meds) Q

    -- initialize the loop variables
    let stacks = replicate m 0i32
    let Q_inds = iota m
    let knns = unflatten m k <| zip (replicate (k*m) (-1)) (replicate (k*m) f32.inf)
    let ordered_all_knns = copy knns

    let res = -- main loop
    loop (ordered_all_knns, knns, leaf_indices, stacks, Q_inds) while (length leaf_indices > 0) do

      -- a. brute-force on previous leaves and ongoing queries
      let knns = map3 (\ q_ind knn leaf_index ->
                    bruteForce Q[q_ind] knn leaf_structure[leaf_index] leaf_index
                  ) Q_inds knns leaf_indices

      -- b. traverse-once to find the next leaves
      let (leaf_indices, stacks) = unzip <| map4 (\ q_ind stack leaf_index wnnd ->
                                            traverse_once height Q[q_ind] stack leaf_index wnnd tree_dims tree_meds
                                          ) Q_inds stacks leaf_indices (map get_wnnd knns)

      -- c. partition so that the queries that finished come last
      let (done_inds, cont_inds) = partition (\i -> leaf_indices[i] == -1) (iota <| length leaf_indices)

      -- d. update the ordered_all_knns for the queries that have finished
      let ordered_all_knns = scatter2D ordered_all_knns
                                (map (\i -> Q_inds[i]) done_inds)
                                (map (\i ->   knns[i]) done_inds)

      -- e. keep only the ongoing parts of the partitioned arrays: Q, knns, leaf_indices, stacks
      let leaf_indices = gather1d cont_inds leaf_indices
      let stacks =       gather1d cont_inds stacks
      let Q_inds =       gather1d cont_inds Q_inds
      let knns =         gather2d cont_inds knns

      -- finish iteration
      in (ordered_all_knns, knns, leaf_indices, stacks, Q_inds)

    -- change the knns p-indices so they point to their original indices and not the sorted ones
    let finished_all_knns = unzip_matrix <| (.0) <| res
    let fixed_knn_inds = gather1d (flatten finished_all_knns.0) original_P_inds |> unflatten m k
    in (fixed_knn_inds, finished_all_knns.1)

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v1 GetLeafSizeLb GetK P Q |> (.1)

-- ==
-- input @ data/test1.in
-- output @ data/test1.out
-- input @ data/test2.in
-- output @ data/test2.out
-- input @ data/test3.in
-- output @ data/test3.out
-- input @ data/test4.in
-- output @ data/test4.out
-- input @ data/test5.in
-- output @ data/test5.out
