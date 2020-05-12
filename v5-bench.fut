-- ==
-- random compiled input { [40000][5]f32  [40000][5]f32 }
-- random compiled input { [80000][5]f32  [80000][5]f32 }
-- random compiled input { [131072][5]f32  [131072][5]f32 }
-- random compiled input { [524288][5]f32  [524288][5]f32 }
-- random compiled input { [1048576][5]f32 [1048576][5]f32 }
-- random compiled input { [2097152][5]f32 [2097152][5]f32 }
-- random compiled input { [4194304][5]f32 [4194304][5]f32 }
-- random compiled input { [8388608][5]f32 [8388608][5]f32 }
-- random compiled input { [8388608][5]f32 [16777216][5]f32 }

open import "v5"

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v5 GetLeafSizeLb GetK P Q |> (.1)
