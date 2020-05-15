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
                           (num_leaves: i32)
                           (tree_dims: [tree_size]i32)
                           (tree_meds: [tree_size]f32)
                           (lbs: [tree_size_plus][d]f32)
                           (ubs: [tree_size_plus][d]f32) =

  let getPackedInd (stk: i32) (level: i32) : bool =
    i32.get_bit level stk |> (>0)
  let setPackedInd (stk: i32) (level: i32) (v: i32) : i32 =
    i32.set_bit level stk v

  let tree_index = leaf_index + tree_size

  let (_, stack, rec_node, _) =
  loop (node_index, stack, rec_node, level) =
       (tree_index, stack, num_leaves, h)
   while (node_index != 0) && (rec_node == num_leaves) do
    let parent_index = getParent node_index
    let sibling_index = getSibling node_index in
    if getPackedInd stack level
    then -- sibling (second node) already visited, go up the tree
      (parent_index, setPackedInd stack level 0, rec_node, level-1)
    else
      --- Test if this or both
      if
        (special_test q wnnd lbs[sibling_index] ubs[sibling_index])
      then (parent_index, setPackedInd stack level 1, sibling_index, level)
      else (parent_index, setPackedInd stack level 0, rec_node, level-1)

  let new_leaf =
    if rec_node == num_leaves
      then num_leaves -- we are done, we are at the root node and its second child has been visited
      else find_natural_leaf rec_node q tree_dims tree_meds
  in (new_leaf, stack)

let sort_by_fst (arr: [](i32, i32)) (num_bits_to_sort: i32) =
  radix_sort_int_by_key (.0) num_bits_to_sort i32.get_bit arr

let v7 [n][m][d] (leaf_size_lb: i32) (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =

    -- pad the array of points
    let pad_elm = replicate d GetPadValue
    let (padded_P, leaf_size) = pad P pad_elm leaf_size_lb

    -- get the height and build the balanced tree
    -- original_P_inds is used to put correct the knns indices in the end
    let height = get_height leaf_size (length padded_P)
    let (tree_dims, tree_meds, global_lbs, global_ubs, leaf_structure, original_P_inds) = build_balanced_tree padded_P height leaf_size

    let num_leaves = length leaf_structure

    let size_promise = length tree_dims
    let tree_dims = tree_dims :> [size_promise]i32
    let tree_meds = tree_meds :> [size_promise]f32

    let size_promise = length global_lbs
    let global_lbs = global_lbs :> [size_promise][d]f32
    let global_ubs = global_ubs :> [size_promise][d]f32

    let num_bits_to_sort = height + 3

    -- find the initial leaves of the queries
    let leaf_indices = map (\q -> find_natural_leaf 0 q tree_dims tree_meds) Q

    -- initialize the loop variables
    let stacks = replicate m 0i32
    let knns = unflatten m k <| zip (replicate (k*m) (-1)) (replicate (k*m) GetPadValue)
    let ordered_all_knns = copy knns

    -- sort the meta-data by leaf-indices
    -- since knns and stacks are all blank, we only need to sort Q and leaf_indices
    let (leaf_indices, sort_order) = unzip <| sort_by_fst (zip_inds leaf_indices) num_bits_to_sort
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
                                            traverse_once height q stack leaf_index wnnd num_leaves tree_dims tree_meds global_lbs global_ubs
                                          ) Q stacks leaf_indices (map get_wnnd knns)

      -- sort leaf_indices
      let (leaf_indices, sort_order) = unzip
          <| sort_by_fst (zip_inds leaf_indices) num_bits_to_sort

      -- find some numbers
      let num_active = length leaf_indices
      let num_done = reduce_comm (+) 0i32 <| map (\lind -> if lind == num_leaves then 1 else 0) leaf_indices
      let num_cont = num_active - num_done
      let leaf_indices = take num_cont leaf_indices

      -- c. partition so that the queries that finished come last
      let (cont_knns, done_knns) = split num_cont <| gather2d sort_order knns
      let (cont_Q_inds, done_Q_inds) = split num_cont <| gather1d sort_order Q_inds

      let done_knns = done_knns :> [num_done][k](i32, f32)
      let done_Q_inds = done_Q_inds :> [num_done]i32
      let ordered_all_knns = scatter2D ordered_all_knns done_Q_inds done_knns

      -- d. gather the stacks and Q's
      let cont_inds = take num_cont sort_order
      let stacks = gather1d cont_inds stacks
      let Q = gather2d cont_inds Q

      -- finish iteration
      in (ordered_all_knns, cont_knns, leaf_indices, stacks, Q, cont_Q_inds)

    -- change the knns p-indices so they point to their original indices and not the sorted ones
    let finished_all_knns = unzip_matrix <| (.0) <| res
    let fixed_knn_inds = gather1d (flatten finished_all_knns.0) original_P_inds |> unflatten m k
    in (fixed_knn_inds, finished_all_knns.1)

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v7 GetLeafSizeLb GetK P Q |> (.1)
