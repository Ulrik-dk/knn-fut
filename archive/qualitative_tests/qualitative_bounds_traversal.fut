open import "../bf"
open import "../constants"
open import "../kd-tree-common"

let v6 [n][m][d] (leaf_size_lb: i32) (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =
    -- pad the array of points
    let (padded_P, leaf_size, num_leaves, height, num_tree_nodes, num_bounds) = pad P leaf_size_lb

    -- original_P_inds is used to put correct the knns indices in the end
    let (tree_dims, tree_meds, global_lbs, global_ubs, leaf_structure, _) =
      build_balanced_tree padded_P num_leaves num_tree_nodes num_bounds height leaf_size

    let num_bits_to_sort = height + 3

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

    let histogram = replicate num_leaves 0
    let iteration = 0

    in (.6) <| -- just return the histogram
    loop (ordered_all_knns, knns, leaf_indices, stacks, Q, Q_inds, histogram, iteration) while (length leaf_indices > 0) do

      -- a. brute-force on previous leaves and ongoing queries
      let knns = map3 (\ q knn leaf_index ->
                    bruteForce q knn leaf_structure[leaf_index] leaf_index
                  ) Q knns leaf_indices

      -- b. traverse-once to find the next leaves
      let (leaf_indices, stacks) = unzip <| map4 (\ q stack leaf_index wnnd ->
                                            traverse_once_mk2 height q stack leaf_index wnnd num_leaves tree_dims tree_meds global_lbs global_ubs
                                          ) Q stacks leaf_indices (map get_wnnd knns)

      let num_active = length leaf_indices

      -- sort leaf_indices
      let (leaf_indices, sort_order) = unzip
        <| sort_by_fst (zip leaf_indices (indices leaf_indices)) num_bits_to_sort

      -- find some numbers
      let num_done = reduce_comm (+) 0i32 <| map (\lind -> if lind == num_leaves then 1 else 0) leaf_indices
      let num_cont = num_active - num_done

      -- c. partition so that the queries that finished come last
      let (cont_inds, done_inds) = split num_cont sort_order

      -- d. gather the stacks and Q's
      let stacks = gather1d cont_inds stacks
      let Q = gather2d cont_inds Q

      -- e. take leaf_indices
      let leaf_indices = take num_cont leaf_indices

      -- f. update the ordered_all_knns for the queries that have finished
      let ordered_all_knns = scatter2d ordered_all_knns
                              (gather1d done_inds Q_inds)
                              (gather2d done_inds knns)

      -- g. finally, gather the rest
      let Q_inds = gather1d cont_inds Q_inds
      let knns = gather2d cont_inds knns

      -- update histogram
      let histogram = histogram with [iteration] = num_cont
      let iteration = iteration + 1

      -- finish iteration
      in (ordered_all_knns, knns, leaf_indices, stacks, Q, Q_inds, histogram, iteration)

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v6 GetLeafSizeLb GetK P Q
