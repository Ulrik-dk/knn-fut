open import "lib/github.com/diku-dk/sorts/merge_sort"
open import "lib/github.com/diku-dk/sorts/radix_sort"
open import "lib/batch-merge-sort"

-- anonymous dimensions so we can gather less than all src elms
let gather1d [n] 't (inds: [n]i32) (src: []t) : [n]t =
  map (\i -> src[i]) inds

let gather2d [n] [d] 't (inds: [n]i32) (src: [n][d]t) : [n][d]t =
  map (\ ind -> map (\j -> src[ind, j]) (iota d)) inds

let my_maxf32 (a: f32) (b: f32) =
    if f32.isinf a then b
    else if f32.isinf b then a
    else f32.max a b

let i32_log2 (x: i32) : i32 =
  i32.f32 <| f32.log2 <| f32.i32 x

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
-- returns ________________
let build_balanced_tree [n][d] (P: [n][d]f32) (h: i32) : ([](i32, f32), [][]f32) =

    -- the number of leaves is determined from the height
    let num_leaves = 1<<(h+1)

    -- the indices of the points
    let Pinds = iota n

    -- the number of nodes in the tree from the number of leaves
    let num_tree_nodes = num_leaves - 1

    -- the tree itself, empty to begin with
    let tree = zip (replicate num_tree_nodes 0i32) (replicate num_tree_nodes 0.0f32)

    -- building the tree and sorting the inds of the points in a loop,
    -- one loop for every level of the tree
    let (tree, Pinds) = loop (tree, Pinds) for depth < (h+1) do

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

      -- the point indices for this segment
      let my_seg_Pindss = map (\i -> seg_Pinds[i]) <| iota seg_cnt

      let (t_inds, dims_medians, sPinds) = unzip3 <| map (\i ->

          -- the actual points in this segment
          let my_seg = gather1d my_seg_Pindss[i] P

          -- TODO: Make reduces commutative
          let my_seg_T = transpose my_seg
          let mins = map (\row -> reduce f32.min f32.highest row) my_seg_T |> intrinsics.opaque
          let maxs = map (\row -> reduce my_maxf32 (row[0]) row) my_seg_T

          -- the vector of differences between the mins and maxs
          let difs = map2(-) mins maxs

          -- the index of the dimension with highest difference between max and min
          -- TODO: make this more elegant
          let (_, dim_ind) = reduce (\(dif1, i1) (dif2, i2) ->
                                  if(dif1 > dif2)
                                    then (dif1, i1)
                                    else (dif2, i2)
                             ) (f32.lowest, -1i32) (zip difs (iota d))

          -- dim_ind values and global indices of my_seg, sorted by the values
          --TODO: FIXME
          let (s_vals, s_inds) = zip (map(\vect -> vect[dim_ind]) my_seg) my_seg_Pindss[i]
                                |> radix_sort_float_by_key (.0) f32.num_bits (f32.get_bit)
                                |> unzip

          -- median value picked from sorted values
          let median = s_vals[(seg_len-1)/2]

          -- index to place this median-value/dim-ind pair into the tree
          -- TODO: is this correct?
          let t_ind = i + seg_cnt - 1
          in (t_ind, (dim_ind, median), s_inds)
        ) <| iota seg_cnt

      -- putting the new tree-medians and dimensions onto the tree
      let tree = scatter tree t_inds dims_medians

      -- continuing the loop
      in (tree, (flatten sPinds))

    -- "sorts" the points P such that they now align with the new ordering of the inds
    let reordered_P = map(\i -> P[i]) Pinds

    -- returns the tree and the reordered points
    in (tree, reordered_P)

let find_natural_leaf [tsz] (Q: f32) (tree: [tsz]f32) : i32 =
    let lid = 0
    let lid = loop lid while (lid < tsz) do
      if (Q <= tree[lid])
        then (lid*2)+1
        else (lid*2)+2
    in lid - tsz

let traverse_once [tsz] (Q: f32) (tree: [tsz]f32) (lidx: i32) (stack: i32) =
  (1i32, stack)
  -- TODO: take from Cosmins code
  -- climb to recursionpoint / root
  -- stop, or climb to new natural leaf
  -- return -1 if processing is done
  -- else, return new lidx corresponding to new leaf, and updated stack

let main [nd] (Q: f32) (P: [nd]f32) (d: i32) (leaf_size_lb: i32) =
    let n = nd / d
    let P = unflatten n d P
    let pad_elm = replicate d f32.inf

    -- pad and shadow out old P and n
    let (P, leaf_size) = pad P pad_elm leaf_size_lb
    let n = length P

    let h = h_from_l_sz leaf_size n
    let (tree, P) = build_balanced_tree P h
    let (tree_dims, tree_medians) = unzip tree
    in (leaf_size, h, n, tree_dims, tree_medians, P)
    --let lidx = find_natural_leaf Q tree
--    let num_leaves = (length tree) + 1
--    let visited = replicate num_leaves 0
--    let stack = 0
--    let (visited, _, _) = loop (visited, stack, lidx) while (lidx != -1) do
--      let visited = visited with [lidx] = 1
--      let (lidx, stack) = traverse_once Q tree lidx stack
--      in (visited, stack, lidx)
--    in visited

-- I dont know how to mix generated and user-defined data in futhark-dataset. The documentation did not help.
entry test [nd] (P: [nd]f32) =
  main 1.0f32 P 16i32 256i32
