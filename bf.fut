open import "lib/github.com/diku-dk/sorts/merge_sort"
--open import "lib/github.com/diku-dk/segmented/segmented"
open import "lib/batch-merge-sort"
open import "util"

let my_dist [d] (p: [d]f32) (q: [d]f32) : f32 =
  f32.abs <| reduce_comm (+) 0f32 <| map2 (-) p q

let pick_cols [n][m] 't (k: i32) (A: [n][m]t) : [n][k]t =
  map (\ row ->
    map (\ i -> row[i]) <| iota k
  ) A

--trivial brute-force knn algorithm
let main [n][m][d] (k: i32) (P: [n][d]f32) (Q: [m][d]f32) =
  let dists_inds = map (\q ->
                    map2 (\p i ->
                      (my_dist p q, i)
                    ) P (iota n)
                   ) Q
  let sorted_dists_inds = batch_merge_sort (f32.inf, i32.highest) (\(a,_) (b,_) -> a <= b) dists_inds
  let (dists, inds) = pick_cols k sorted_dists_inds |> unzip_matrix
  in (dists, inds)
