open import "lib/github.com/diku-dk/sorts/merge_sort"
open import "lib/github.com/diku-dk/sorts/radix_sort"

let get 't [n] (inds: [n]i32) (src: []t) : [n]t =
    map (\i -> src[i]) inds

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

-- TODO: take arbitrary vectors in P and make the tree say something about the dimension
let build_balanced_tree [n] (P: [n]f32) (h: i32) : ([]f32, []f32) =
    let num_leaves = 1<<(h+1)
    let num_tree_nodes = num_leaves - 1
    let tree = replicate num_tree_nodes 0
    let (tree, P) = loop (tree, P) for depth < (h+1) do
      let seg_len = n >> depth
      let seg_cnt = n / seg_len
      let T_ofs = seg_cnt-1
      let sorted = unflatten seg_cnt seg_len P |> map (radix_sort_float f32.num_bits (f32.get_bit))
      let inds = seg_cnt |> iota |> map (+T_ofs) :> [seg_cnt]i32 --coerce
      let tree = scatter tree inds (map (\arr -> arr[(seg_len-1) / 2]) sorted)
      in (tree, (flatten sorted))
    in (tree, P)

let find_natural_leaf [tsz] (Q: f32) (tree: [tsz]f32) : i32 =
    let lid = 0
    let lid = loop lid while (lid < tsz) do
      if (Q <= tree[lid])
        then (lid*2)+1
        else (lid*2)+2
    in lid - tsz

let traverse_once [tsz] (Q: f32) (tree: [tsz]f32) (lidx: i32) (stack: i32) =
  (1, stack)
  -- TODO: take from Cosmins code
  -- climb to recursionpoint / root
  -- stop, or climb to new natural leaf
  -- return -1 if processing is done
  -- else, return new lidx corresponding to new leaf, and updated stack

let main [n] (Q: f32) (P: [n]f32) (d: i32) (leaf_size_lb: i32) =
    --let n = nd / d
    --let P = unflatten n d P
    let pad_elm = f32.inf

    -- pad and shadow out old P and n
    let (P, leaf_size) = pad P pad_elm leaf_size_lb
    let n = length P

    let h = h_from_l_sz leaf_size n
    let (tree, P) = build_balanced_tree P h
    --let lidx = find_natural_leaf Q tree
    in (leaf_size, h, n, tree, P)
--    let num_leaves = (length tree) + 1
--    let visited = replicate num_leaves 0
--    let stack = 0
--    let (visited, _, _) = loop (visited, stack, lidx) while (lidx != -1) do
--      let visited = visited with [lidx] = 1
--      let (lidx, stack) = traverse_once Q tree lidx stack
--      in (visited, stack, lidx)
--    in visited
