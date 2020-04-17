-- for common functions that are not algorithm specific
-- anonymous dimensions so we can gather less than all src elms

let my_dist [d] (p: [d]f32) (q: [d]f32) : f32 =
  --TODO: is this the correct? Why not just use the strict euclidian distance?
    -- Perhaps use some simple distance function that gives the same ordering as euclidian,
    -- and then have a function at the end that converts to the correct distance, for result-output
  f32.abs <| reduce_comm (+) 0f32 <| map2 (-) p q

let cp 't [n] (arr: [n]t) (ind: i32) (e: t) : [n]t =
  -- TODO: this should not be used, but the native in-place updates should be used
  -- however, I cant get that to work everywhere
  map(\i -> if i != ind then arr[i] else e) <| iota n


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
