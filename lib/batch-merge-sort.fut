let log2 (n: i32) : i32 =
  let r = 0
  let (r, _) = loop (r,n) while 1 < n do
    let n = n / 2
    let r = r + 1
    in (r,n)
  in r

local let ensure_pow_2 (n: i32) : (i32, i32) =
  let d_floor = log2 n
  in if n==0 then (0, 0)
     else if (1 << d_floor) == n
         then (d_floor, 0)
         else let d = d_floor+1
              in  (d, (1<<d) - n)

local let kernel_par [n] 't ((<=): t -> t -> bool) (a: [n]t) (p: i32) (q: i32) : *[n]t =
  let d = 1 << (p-q) in
  map (\i -> unsafe
             let a_i = a[i]
             let up1 = ((i >> p) & 2) == 0
             in
             if (i & d) == 0
             then let a_iord = a[i | d] in
                  if a_iord <= a_i == up1
                  then a_iord else a_i
             else let a_ixord = a[i ^ d] in
                      if a_i <= a_ixord == up1
                      then a_ixord else a_i)
      (iota n)

let batch_merge_sort [m][n] 't (largest: t) ((<=): t -> t -> bool) (xss: [m][n]t): *[m][n]t =
  let (d, pad) = ensure_pow_2 n
  let pad_len = pad + n
  let xss_pad = map(\row -> 
                        map  (\i -> if i < n then row[i] else largest)
                             (iota pad_len)
                   ) xss
  -- sort all padded rows, the largest should be in
  -- number of pad at the end of the sorted array 
  let res = 
    loop (xss_pad) for i < d do
      loop (xss_pad) for j < i+1 do
        map (\row -> kernel_par (<=) row i j) xss_pad
  -- get rid of the padding
  in  map (\row -> map (\i -> row[i]) (iota n) ) res
  
