-- ==
-- compiled input @ data/test1.in output @ data/test1.out
-- compiled input @ data/test2.in output @ data/test2.out
-- compiled input @ data/test3.in output @ data/test3.out
-- compiled input @ data/test4.in output @ data/test4.out
-- compiled input @ data/test5.in output @ data/test5.out
-- compiled input @ data/test6.in output @ data/test6.out

open import "v1"

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v1 GetLeafSizeLb GetK P Q |> (.1)
