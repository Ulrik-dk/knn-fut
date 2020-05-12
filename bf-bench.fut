-- ==
-- random compiled input { [40000][5]f32  [40000][5]f32 }
-- random compiled input { [80000][5]f32  [80000][5]f32 }
-- random compiled input { [131072][5]f32  [131072][5]f32 }
-- random compiled input { [524288][5]f32  [524288][5]f32 }

open import "bf"

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  runBF P Q
