-- ==
-- random compiled input { [16384][5]f32  [16384][5]f32 }
-- random compiled input { [32768][5]f32  [32768][5]f32 }
-- random compiled input { [65532][5]f32  [65532][5]f32 }
-- random compiled input { [131072][5]f32  [131072][5]f32 }
-- random compiled input { [262144][5]f32  [262144][5]f32 }
-- random compiled input { [524288][5]f32  [524288][5]f32 }
-- random compiled input { [1048576][5]f32 [1048576][5]f32 }
-- random compiled input { [2097152][5]f32 [2097152][5]f32 }
-- random compiled input { [4194304][5]f32 [4194304][5]f32 }
-- random compiled input { [8388608][5]f32 [8388608][5]f32 }
-- random compiled input { [16777216][5]f32 [16777216][5]f32 }

open import "v3"

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v3 GetLeafSizeLb GetK P Q |> (.1)
