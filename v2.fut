open import "lib/batch-merge-sort"
open import "bf"
open import "constants"
open import "kd-tree-common"

let v2 [n][m][d] (leaf_size_lb: i32) (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =
    let pad_elm = replicate d f32.inf

    let (padded_P, leaf_size) = pad P pad_elm leaf_size_lb

    let h = get_height leaf_size (length padded_P)
    let (tree, reordered_P, original_P_inds) = build_balanced_tree padded_P h

    let num_leaves = (length padded_P) / leaf_size

    let leaf_structure = unflatten_3d num_leaves leaf_size d(flatten reordered_P)

    let (tree_dims, tree_meds, _, _) = unzip4 tree

    let leaf_indices = map (\q -> find_natural_leaf 0 q tree_dims tree_meds) Q

    let stacks = replicate m 0i32

    let Q_inds = iota m
    let knns = unflatten m k <| zip (replicate (k*m) (-1)) (replicate (k*m) f32.inf)
    let ordered_all_knns = copy knns
    let ordered_all_knns = unzip_matrix <| (.0) <|
    loop (ordered_all_knns, knns, leaf_indices, stacks, Q_inds) while (length leaf_indices > 0) do

      -- 1. brute-force on previous leaves and ongoing queries
      let knns = map3 (\ q_ind knn leaf_index ->
                    bruteForce Q[q_ind] knn leaf_structure[leaf_index] leaf_index
                  ) Q_inds knns leaf_indices

      -- 2. traverse-once to find the next leaves
      let (leaf_indices, stacks) = unzip <| map4 (\ q_ind stack leaf_index wnnd ->
                                            traverse_once h Q[q_ind] stack leaf_index wnnd tree
                                          ) Q_inds stacks leaf_indices (map get_wnnd knns)

      -- 3. partition so that the queries that finished come last
      let (done_inds, cont_inds) = partition (\i -> leaf_indices[i] == -1) (iota <| length Q_inds)

      -- 4. update the ordered_all_knns for the queries that have finished
      let ordered_all_knns = scatter2D ordered_all_knns
                                (map (\i -> Q_inds[i]) done_inds)
                                (map (\i ->   knns[i]) done_inds)

      -- 5. keep only the ongoing parts of the partitioned arrays: Q, knns, leaf_indices, stacks
      let new_len = length cont_inds
      let knns = gather2d cont_inds knns :> [new_len][k](i32, f32)
      let leaf_indices = gather1d cont_inds leaf_indices
      let stacks = gather1d cont_inds stacks
      let Q_inds = gather1d cont_inds Q_inds
      in (ordered_all_knns, knns, leaf_indices, stacks, Q_inds)

    let fixed_knn_inds = gather1d (flatten ordered_all_knns.0) original_P_inds |> unflatten m k
    in (fixed_knn_inds, ordered_all_knns.1)

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v2 GetLeafSizeLb GetK P Q |> (.1)

-- ==
-- input @ data/test1.in
-- output @ data/test1.out
