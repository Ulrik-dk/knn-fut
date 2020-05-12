open import "bf"
open import "v1"
open import "v2"
open import "v3"
open import "v4"
open import "v5"

entry main_bf [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  runBF P Q
entry main_v1 [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v1 GetLeafSizeLb GetK P Q |> (.1)
entry main_v2 [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v2 GetLeafSizeLb GetK P Q |> (.1)
entry main_v3 [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v3 GetLeafSizeLb GetK P Q |> (.1)
entry main_v4 [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v4 GetLeafSizeLb GetK P Q |> (.1)
entry main_v5 [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v5 GetLeafSizeLb GetK P Q |> (.1)

-- ==
-- compiled random input { [131072][5]f32  [131072][5]f32 }
-- compiled random input { [131072][5]f32  [131072][5]f32 }
-- compiled random input { [524288][5]f32  [524288][5]f32 }
-- compiled random input { [1048576][5]f32 [1048576][5]f32 }
-- compiled random input { [2097152][5]f32 [2097152][5]f32 }
-- compiled random input { [4194304][5]f32 [4194304][5]f32 }
-- compiled random input { [8388608][5]f32 [8388608][5]f32 }
-- compiled random input { [8388608][5]f32 [16777216][5]f32 }
