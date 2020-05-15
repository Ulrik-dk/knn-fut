open import "lib/github.com/diku-dk/sorts/radix_sort"
---- for functions that are used often but abstract

let sort_by_fst (arr: [](i32, i32)) (num_bits_to_sort: i32) =
  radix_sort_int_by_key (.0) num_bits_to_sort i32.get_bit arr

let euclidian_distance [d] (p: [d]f32) (q: [d]f32) : f32 =
  let acc = loop acc = 0 for i < d do
    let dif = (p[i] - q[i])
    in acc + (dif*dif)
  in f32.sqrt acc

let gather1d [n] 't (inds: [n]i32) (src: []t) : [n]t =
  map (\i -> src[i]) inds

let gather2d [n][d] 't (inds: [n]i32) (src: [][d]t) : [][d]t =
  map (\i -> map (\j -> src[i, j]) (iota d)) inds

let scatter2d [m][k][n] 't (arr2d: *[m][k]t) (qinds: [n]i32) (vals2d: [n][k]t) : *[m][k]t =
  let nk = n*k
  let flat_qinds = map (\i -> let (d,r) = (i / k, i % k)
                              in qinds[d]*k + r
                       ) (iota nk)
  let res1d = scatter (flatten arr2d) flat_qinds ((flatten vals2d) :> [nk]t)
  in  unflatten m k res1d

let unzip_matrix [n] [m] 't1 't2 (A: [n][m](t1, t2)) : ([n][m]t1, [n][m]t2) =
  let flat = flatten A
  let (A_1, A_2) = unzip flat
  in (unflatten n m A_1, unflatten n m A_2)

let i32_log2 (x: i32) : i32 =
  i32.f32 <| f32.log2 <| f32.i32 x
