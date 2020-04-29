open import "util"
open import "constants"

let update_knn [k] (knn: [k](i32,f32)) (elm: (i32,f32)) : [k](i32,f32) =
  (.0) <| loop (knn, elm) = (copy knn, elm) for j < k do
    if knn[j].1 < elm.1
      then (knn, elm)
      else let tmp = knn[j]
           let knn[j] = elm
           in (knn, tmp)

let bruteForce [n][d][k] (q: [d]f32)
                         (knn: [k](i32,f32))
                         (refs: [n][d]f32)
                         (leaf_index: i32)
                         : [k](i32, f32) =
  loop knn for i < n do
    let dist = my_dist refs[i] q in
    if dist >= knn[k-1].1
      then knn
      else update_knn knn (((leaf_index*n)+i), dist)

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  let k = GetK
  let knns = unflatten m k <| zip (replicate (m*k) i32.highest) (replicate (m*k) f32.inf) :> *[m][k](i32,f32)
  in map2 (\q knn -> bruteForce q knn P 0) Q knns |> unzip_matrix
