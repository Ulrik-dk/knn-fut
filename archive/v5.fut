open import "bf"
open import "constants"
open import "kd-tree-common"

let v5 [n][m][d] (leaf_size_lb: i32) (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =
    -- pad the array of points
    let (padded_P, leaf_size, num_leaves, height, num_tree_nodes, num_bounds) = pad P leaf_size_lb

    -- original_P_inds is used to put correct the knns indices in the end
    let (tree_dims, tree_meds, global_lbs, global_ubs, leaf_structure, original_P_inds) =
      build_balanced_tree padded_P num_leaves num_tree_nodes num_bounds height leaf_size

    let num_bits_to_sort = height + 2

    -- find the initial leaves of the queries
    let leaf_indices = map (\q -> find_natural_leaf 0 q tree_dims tree_meds) Q

    -- initialize the loop variables
    let stacks = replicate m 0i32
    let knns = unflatten m k <| zip (replicate (k*m) (-1)) (replicate (k*m) GetPadValue)
    let ordered_all_knns = copy knns

    -- since knns and stacks are all blank, we only need to sort Q and leaf_indices
    let (leaf_indices, sort_order) = unzip <| sort_by_fst (zip leaf_indices (iota m)) num_bits_to_sort
    let Q = gather2d sort_order Q
    let Q_inds = sort_order

    let res = -- main loop
    loop (ordered_all_knns, knns, leaf_indices, stacks, Q, Q_inds) while (length leaf_indices > 0) do

      -- a. brute-force on previous leaves and ongoing queries
      let knns = map3 (\ q knn leaf_index ->
                    bruteForce q knn leaf_structure[leaf_index] leaf_index
                  ) Q knns leaf_indices

      -- b. traverse-once to find the next leaves
      let (leaf_indices, stacks) = unzip <| map4 (\ q stack leaf_index wnnd ->
                                            traverse_once_mk2 height q stack leaf_index wnnd (-1) tree_dims tree_meds global_lbs global_ubs
                                          ) Q stacks leaf_indices (map get_wnnd knns)

      -- c. partition so that the queries that finished come last
      let (done_inds, cont_inds) = partition (\i -> leaf_indices[i] == -1) (indices leaf_indices)

      -- d. update the ordered_all_knns for the queries that have finished
      let ordered_all_knns = scatter2d ordered_all_knns
                              (gather1d done_inds Q_inds)
                              (gather2d done_inds knns)

      -- e. keep only the ongoing parts of the partitioned arrays: Q, knns, leaf_indices, stacks
      -- we want to recover cont_inds of knns, leaf_indices, stacks, Q_inds, Q
      -- but we want to place them, not in the order that cont_inds are in now,
      -- but in the order they will be in when we sort them according to leaves

      -- 1. gather leaf_indices
      let leaf_indices = gather1d cont_inds leaf_indices

      -- 2. sort leaf_indices and reorder cont_inds according to this
      let (leaf_indices, sort_order) = unzip
        <| sort_by_fst (zip leaf_indices (indices leaf_indices)) num_bits_to_sort

      let cont_inds = gather1d sort_order cont_inds

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
  v5 GetLeafSizeLb GetK P Q |> (.1)
