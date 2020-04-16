open import "lib/github.com/diku-dk/sorts/merge_sort"
open import "lib/batch-merge-sort"
open import "util"

let my_dist [d] (p: [d]f32) (q: [d]f32) : f32 =
  --TODO: is this the correct? Why not just use the strict euclidian distance?
    -- Perhaps use some simple distance function that gives the same ordering as euclidian,
    -- and then have a function at the end that converts to the correct distance, for result-output
  f32.abs <| reduce_comm (+) 0f32 <| map2 (-) p q

let cp 't [n] (arr: [n]t) (ind: i32) (e: t) : [n]t =
  -- TODO: this should not be used, but the native in-place updates should be used
  -- however, I cant get that to work everywhere
  map(\i -> if i != ind then arr[i] else e) <| iota n


  -- TODO: clean this up
let update_knns [k] (knn: [k](i32,f32))
                    (elm: (i32,f32)) : [k](i32,f32) =
    (.1) <|
    loop (elm, knn) for j < k do
      if knn[j].1 < elm.1
        then (elm, knn)
        else let tmp = knn[j]
             let knn = cp knn j elm
             --let knn[j] = elm -- TODO: make this use in-place updates rather than a copy
             in (tmp, knn)

-- TODO: clean this up
let bruteForce [n][d][k] (q: [d]f32)
                         (knn: [k](i32,f32))
                         (refs: [][d]f32)
                         (ref_inds: [n]i32)
                         (ref_o_inds: [n]i32) : [k](i32, f32) =
  loop knn for i < n do
    let dist = my_dist refs[ref_inds[i]] q in
    if dist >= knn[k-1].1
      then knn
      else update_knns knn (ref_o_inds[i], dist)

--trivial brute-force knn algorithm
let main [n][m][d] (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =
  let knns = unflatten m k <| zip (replicate (m*k) i32.highest) (replicate (m*k) f32.inf) :> *[m][k](i32,f32)
  let Pinds = iota n
  in map2 (\q knn -> bruteForce q knn P Pinds Pinds) Q knns
  |> unzip_matrix
