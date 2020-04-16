-- for common functions that are not algorithm specific
-- anonymous dimensions so we can gather less than all src elms
let gather1d [n] 't (inds: [n]i32) (src: []t) : [n]t =
  map (\i -> src[i]) inds

let gather2d [n][d] 't (inds: [n]i32) (src: [][d]t) : [][d]t =
  map (\ ind -> map (\j -> src[ind, j]) (iota d)) inds

let unzip_matrix [n] [m] 't1 't2 (A: [n][m](t1, t2)) : ([n][m]t1, [n][m]t2) =
  let nm = n*m
  let flat = flatten A :> [nm](t1, t2)
  let (A_1, A_2) = unzip flat
  in (unflatten n m A_1, unflatten n m A_2)

let i32_log2 (x: i32) : i32 =
  i32.f32 <| f32.log2 <| f32.i32 x
