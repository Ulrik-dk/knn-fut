open import "lib/github.com/diku-dk/sorts/merge_sort"
open import "lib/batch-merge-sort"
open import "util"

-- TODO: clean this up
let update_knns [k] (knn: [k](i32,f32))
                    (elm: (i32,f32)) : [k](i32,f32) =
    (.0) <| loop (knn, elm) for j < k do
      if knn[j].1 < elm.1
        then (knn, elm)
        else let tmp = knn[j]
             let knn = cp knn j elm
             --let knn[j] = elm -- TODO: make this use in-place updates rather than a copy
             in (knn, tmp)

-- TODO: clean this up
let bruteForce [n][d][k] (q: [d]f32)
                         (knn: [k](i32,f32))
                         (refs: [][d]f32)
                         (ref_inds: [n]i32)
                         (ref_o_inds: [n]i32) : [k](i32, f32) =
  loop knn for i < n do
    let dist = my_dist refs[ref_inds[i]] q in
    if dist >= knn[k-1].1 -- >= makes it perform as few updates as possible
      then knn
      else update_knns knn (ref_o_inds[i], dist)

-- TODO: call this something else
let simpleBruteForce [n][m][d] (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =
  let knns = unflatten m k <| zip (replicate (m*k) i32.highest) (replicate (m*k) f32.inf) :> *[m][k](i32,f32)
  let Pinds = iota n
  in map2 (\q knn -> bruteForce q knn P Pinds Pinds) Q knns
  |> unzip_matrix

--trivial brute-force knn algorithm
let main [n][m][d] (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =
  simpleBruteForce k P Q
