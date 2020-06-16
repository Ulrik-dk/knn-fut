-- ==
-- random compiled input { [131072][5]f32  [131072][5]f32 }
-- random compiled input { [262144][5]f32  [262144][5]f32 }
-- random compiled input { [524288][5]f32  [524288][5]f32 }
-- random compiled input { [1048576][5]f32 [1048576][5]f32 }
-- random compiled input { [2097152][5]f32 [2097152][5]f32 }


open import "v3"

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v3 GetLeafSizeLb GetK P Q |> (.1)
