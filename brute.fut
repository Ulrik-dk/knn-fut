open import "lib/github.com/diku-dk/sorts/merge_sort"
--open import "lib/github.com/diku-dk/segmented/segmented"

let ex_scn (arr: []i32) = [0] ++ (init (scan (+) 0 arr))


let main [nd] [md] (d: i32) (fP: [nd]i32) (fQ: [md]i32) : ([]i32, []i32) =
  let n = nd / d --8
  let P = unflatten n d fP
  let m = md / d --8
  let Q = unflatten m d fQ
