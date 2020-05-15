-- for functions that are general
let zip_inds [n] 't (arr: [n]t) : ([n](t, i32)) =
  zip arr (iota n)

let my_dist [d] (p: [d]f32) (q: [d]f32) : f32 =
  let acc = loop acc = 0 for i < d do
    let dif = (p[i] - q[i])
    in acc + (dif*dif)
  in f32.sqrt acc

let gather1d [n] 't (inds: [n]i32) (src: []t) : [n]t =
  map (\i -> src[i]) inds

let gather2d [n][d] 't (inds: [n]i32) (src: [][d]t) : [][d]t =
  map (\i -> map (\j -> src[i, j]) (iota d)) inds

let scatter2D [m][k][n] 't (arr2D: *[m][k]t) (qinds: [n]i32) (vals2D: [n][k]t) : *[m][k]t =
  let nk = n*k
  let flat_qinds = map (\i -> let (d,r) = (i / k, i % k)
                              in qinds[d]*k + r
                       ) (iota nk)
  let res1D = scatter (flatten arr2D) flat_qinds ((flatten vals2D) :> [nk]t)
  in  unflatten m k res1D

let unzip_matrix [n] [m] 't1 't2 (A: [n][m](t1, t2)) : ([n][m]t1, [n][m]t2) =
  let nm = n*m
  let flat = flatten A :> [nm](t1, t2)
  let (A_1, A_2) = unzip flat
  in (unflatten n m A_1, unflatten n m A_2)

let i32_log2 (x: i32) : i32 =
  i32.f32 <| f32.log2 <| f32.i32 x
