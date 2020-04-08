open import "lib/github.com/diku-dk/sorts/merge_sort"
open import "lib/github.com/diku-dk/sorts/radix_sort"
open import "lib/batch-merge-sort"

-- anonymous dimensions so we can gather less than all src elms
let gather1d [n] 't (inds: [n]i32) (src: []t) : [n]t =
  map (\i -> src[i]) inds

let gather2d [n] [d] 't (inds: [n]i32) (src: [n][d]t) : [n][d]t =
  map (\ ind -> map (\j -> src[ind, j]) (iota d)) inds

let unzip_matrix [n] [m] 't1 't2 (A: [n][m](t1, t2)) : ([n][m]t1, [n][m]t2) =
  let nm = n*m
  let flat = flatten A :> [nm](t1, t2)
  let (A_1, A_2) = unzip flat
  in (unflatten n m A_1, unflatten n m A_2)

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

      -- the point indices for this segment
      -- COSMIN: this is map with identity, why? 
      -- let my_seg_Pindss = map (\i -> seg_Pinds[i]) <| iota seg_cnt
      let my_seg_Pindss = seg_Pinds

      -- the actual points in this segment
      -- COSMIN: this is a performance bug as it does not exploits
      --         the inner parallelism of size d; fixed below; please
      --         verify that it is correct.
      -- let my_segs = map (\i -> gather1d my_seg_Pindss[i] P) <| iota seg_cnt
      let my_segs = map (\sgm_inds -> 
                            map (\ind -> 
                                    map (\j -> P[ind, j]) (iota d)
                                ) sgm_inds
                        ) my_seg_Pindss

      -- for every segment, the dimension chosen
      let (_, dim_inds) =
                  unzip <|
                  map (\i ->
                    let my_seg_T = transpose my_segs[i] |> intrinsics.opaque
                    let mins = map (\row -> reduce_comm f32.min f32.highest row) my_seg_T |> intrinsics.opaque
                    let maxs = map (\row -> reduce_comm my_maxf32 f32.lowest row) my_seg_T |> intrinsics.opaque
                    let difs = map2(-) mins maxs |> intrinsics.opaque
                    in reduce_comm (\(dif1, i1) (dif2, i2) ->
                      if(dif1 > dif2)
                        then (dif1, i1)
                        else (dif2, i2)
                      ) (f32.lowest, -1i32) (zip difs (iota d))
                  ) <| iota seg_cnt

      let valss_indss = map (\i -> zip (map(\p -> p[dim_inds[i]]) my_segs[i]) my_seg_Pindss[i] ) <| iota seg_cnt

      -- the index is only a passanger, so the value we put in the neutral element does not matter
      -- COSMIN: here (f32.highest, seg_len) are the elements by which mergeSorts
      --         pads to a power of two: you need to make sure they are ordered at the end!
      let (s_valss, s_indss) = unzip_matrix <| batch_merge_sort (f32.highest, seg_len) 
                                                                (\(a,i1) (b,i2) -> if a < b then true  else
                                                                                   if a > b then false else
                                                                                   i1 <= i2
                                                                ) valss_indss

      -- COSMIN: the map below was also returning sPinds which was essentially s_indss,
      --         and also the dim_inds. I have removed them from the map as they are
      --         redundant computation.
      let sPinds = s_indss
      let (t_inds, dims_medians) = unzip <| map (\i ->
          -- median value picked from sorted values
          let median = s_valss[i,(seg_len-1)/2]

          -- index to place this median-value/dim-ind pair into the tree
          let t_ind = i + seg_cnt - 1 -- FIXME: is this correct? COSMIN: looks correct
          in (t_ind, median)
        ) <| iota seg_cnt

      -- putting the new tree-medians and dimensions onto the tree
      let tree = scatter tree t_inds (zip dim_inds dims_medians)

      -- continuing the loop
      in (tree, (flatten sPinds))

    -- "sorts" the points P such that they now align with the new ordering of the inds
    -- COSMIN: use gather2D here to utilize also the inner parallelism of size d!
    let reordered_P = gather2d (Pinds :> [n]i32) P

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
--
entry main [n][d] (leaf_size_lb: i32) (P: [n][d]f32) =
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
entry test [n][d] (P: [n][d]f32) =
  main 256i32 P
