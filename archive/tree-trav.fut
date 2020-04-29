-- ==
-- entry: main
--
-- compiled input { 3i32 1024i32 8024.0f32 3000.0f32 }
-- output { [8192.0f32, 4096.0, 12288.0, 2048.0, 6144.0, 10240.0, 14336.0, 1024.0, 3072.0, 5120.0, 7168.0, 9216.0, 11264.0, 13312.0, 15360.0] [22i32, 21, 20, 19, 23, 24, 25, -1, -1, -1, -1, -1, -1, -1, -1, -1] 7i32 }

let getParent (node_index: i32) = (node_index-1) / 2
-- uneven node indices are left-children, even are right-children
let getLeftChild (node_index: i32) = (node_index * 2) + 1
let getRightChild (node_index: i32) = (node_index * 2) + 2
let getSibling (node_index: i32) = if (node_index%2==0) then node_index-1 else node_index+1

let isLeaf (h: i32) (node_index: i32) = node_index >= (1 << (h+1)) - 1

let getQuerriedLeaf (h: i32) (ppl: i32) (q: f32) =
    let leaf_ind = (t32 q) / ppl
    in  leaf_ind + (1<<(h+1)) - 1

-- This is implemented for 1-dim
-- height: the height of the tree (without leaves)
-- tree: the k-d tree, denoted `split_values` in Fabian's code
-- query: our query (1-dim)
-- knn:    the worse nearest neighbor
-- last_leaf: last visited leaf
-- stack:  array of booleans (which should be represented as an int)
--         which denotes whether the `second child` of a node has
--         been visited already
-- Results: the index of the new leaf and the new stack
let traverseOnce (height: i32) (tree:    []f32)
                 (query: f32) (knn:       f32)
                 (last_leaf: i32)
                 (stack: i32) : (i32, i32) =

  -- trivial functions for reading/writting from the stack,
  -- which is maintained as an array of booleans.
  let getPackedInd (stk: i32) (level: i32) : bool =
    i32.get_bit level stk |> (>0)
  let setPackedInd (stk: i32) (level: i32) (v: bool) : i32 =
    i32.set_bit level stk (if v then 1 else 0)

  -- level is actually that of parent_index into the stack?
  let (parent_rec, stack, _, rec_node) =
      loop (node_index, stack, level, rec_node) =
           (last_leaf, stack, height, -1)
            while (node_index != 0) && (rec_node < 0) do
                let parent_index = getParent node_index
                let sibling_index = getSibling node_index
                in if (!(getPackedInd stack level) && f32.abs(query - tree[parent_index]) < knn)
                  then (parent_index, setPackedInd stack level true, level, sibling_index)
                  else (parent_index, setPackedInd stack level false, level-1, rec_node)

  let new_leaf =
      if parent_rec == 0 && rec_node == -1
      then -1 -- we are done, we are at the root node and its second child has been visited
      else loop node_index = rec_node
        while !(isLeaf height node_index) do
          if query < tree[node_index]
            then getLeftChild node_index -- if query less than median, go left
            else getRightChild node_index -- if query greater than median, go right
  in (new_leaf, stack)

-- h:   height of the tree (excluding the leaves)
-- ppl: # of points per leaf
-- q:   the query
-- knn: the worse nearest neighbor
-- run with: echo "3 1024 8024.0f32 3000.0f32" | ./tree-trav
entry main (h: i32) (ppl: i32) (q: f32) (knn: f32)=
  let num_nodes = (1 << (h+1)) - 1
  let num_leaves=  1 << (h+1)

  -- build a naive tree that contains the values: [0 ... num_leaves * ppl - 1]
  let tree_arr  =
      loop (tree) = (replicate num_nodes 0.0f32)
      for i < (h+1) do
        let beg = (1 << i) - 1
        let len = 1 << i
        let inds = map (+beg) (iota len)
        let num_leaves_per_parent = num_leaves / len
        let num_points_per_parent = num_leaves_per_parent * ppl
        let vals = map (\k -> r32 ((2*k+1) * num_points_per_parent / 2)) (iota len)
        in  scatter tree inds vals
  -- get the querried leaf
  let q_leaf = getQuerriedLeaf h ppl q

  let visits = replicate num_leaves (-1)
  let visits[0] = q_leaf

  -- propagate the query `q` through the tree
  let (visits, _, _, loop_count) =
    loop (visits, stack, last_leaf, i) =
         (visits, 0, q_leaf, 0)
       --(visits, 0i32, q_leaf, 0)
    while last_leaf != -1 do
      let (new_leaf, stack) =
          traverseOnce h tree_arr q knn last_leaf stack

      let visits = if new_leaf != -1
                   then let visits[i+1] = new_leaf in visits
                   else visits
      in  (visits, stack, new_leaf, i+1)

  in (tree_arr, visits, loop_count)
