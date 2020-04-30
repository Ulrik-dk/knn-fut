open import "lib/github.com/diku-dk/sorts/merge_sort"
open import "lib/github.com/diku-dk/sorts/quick_sort"
open import "lib/github.com/diku-dk/sorts/radix_sort"
open import "lib/batch-merge-sort"
open import "bf"
open import "constants"
open import "kd-tree-common"

let v4 [n][m][d] (leaf_size_lb: i32) (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =

    -- pad the array of points
    let pad_elm = replicate d f32.inf
    let (padded_P, leaf_size) = pad P pad_elm leaf_size_lb

    -- get the height and build the balanced tree
    -- original_P_inds is used to put correct the knns indices in the end
    let height = get_height leaf_size (length padded_P)
    let (tree_quadruple, leaf_structure, original_P_inds) = build_balanced_tree padded_P height leaf_size
    let (tree_dims, tree_meds, _, _) = unzip4 tree_quadruple

    -- V4 addition start
    let num_bits_to_sort = height + 2
    let sort_by_fst (arr: [](i32, i32)) =
      radix_sort_int_by_key (.0) num_bits_to_sort i32.get_bit arr
      --merge_sort_by_key (.0) (<=) arr
    -- V4 addition end


    -- find the initial leaves of the queries
    let leaf_indices = map (\q -> find_natural_leaf 0 q tree_dims tree_meds) Q

    -- initialize the loop variables
    let stacks = replicate m 0i32
    let knns = unflatten m k <| zip (replicate (k*m) (-1)) (replicate (k*m) f32.inf)
    let ordered_all_knns = copy knns

    -- V2 addition begin
    -- sort the meta-data by leaf-indices
    -- since knns and stacks are all blank, we only need to sort Q and leaf_indices
    let (leaf_indices, sort_order) = unzip <| sort_by_fst (zip leaf_indices (iota m))
    -- V2 addition end
    -- V3 addition begin
    let Q = gather2d sort_order Q
    -- V3 addition end

    let res = -- main loop
    loop (ordered_all_knns, knns, leaf_indices, stacks, Q) while (length leaf_indices > 0) do

      -- a. brute-force on previous leaves and ongoing queries
      let knns = map3 (\ q knn leaf_index ->
                    bruteForce q knn leaf_structure[leaf_index] leaf_index
                  ) Q knns leaf_indices

      -- b. traverse-once to find the next leaves
      let (leaf_indices, stacks) = unzip <| map4 (\ q stack leaf_index wnnd ->
                                            traverse_once height q stack leaf_index wnnd tree_dims tree_meds
                                          ) Q stacks leaf_indices (map get_wnnd knns)

      -- c. partition so that the queries that finished come last
      let (done_inds, cont_inds) = partition (\i -> leaf_indices[i] == -1) (iota <| length leaf_indices)

      -- d. update the ordered_all_knns for the queries that have finished
      let ordered_all_knns = scatter2D ordered_all_knns
                                done_inds
                                (map (\i ->   knns[i]) done_inds)

      -- e. keep only the ongoing parts of the partitioned arrays: Q, knns, leaf_indices, stacks
      ------- V2 addition/modification begin
      -- we want to recover cont_inds of knns, leaf_indices, stacks, Q
      -- but we want to place them, not in the order that cont_inds are in now,
      -- but in the order they will be in when we sort them according to leaves

      -- 1. gather leaf_indices
      let num_active = length cont_inds

      let leaf_indices = gather1d cont_inds leaf_indices :> [num_active]i32

      -- 2. sort leaf_indices and reorder cont_inds according to this
      let (leaf_indices, sort_order) = unzip
          <| sort_by_fst (zip leaf_indices (iota num_active))

      let cont_inds = gather1d sort_order cont_inds

      -- V3 addition begin
      let Q = gather2d cont_inds Q
      -- V3 addition end

      -- 3. finally, gather using this reordered cont_inds array
      ------- V2 addition/modification end
      let stacks =       gather1d cont_inds stacks
      let knns =         gather2d cont_inds knns

      -- finish iteration
      in (ordered_all_knns, knns, leaf_indices, stacks, Q)

    -- change the knns p-indices so they point to their original indices and not the sorted ones
    let finished_all_knns = unzip_matrix <| (.0) <| res
    let fixed_knn_inds = gather1d (flatten finished_all_knns.0) original_P_inds |> unflatten m k
    in (fixed_knn_inds, finished_all_knns.1)

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v4 GetLeafSizeLb GetK P Q |> (.1)

-- ==
-- input @ data/test1.in
-- output @ data/test1.out
