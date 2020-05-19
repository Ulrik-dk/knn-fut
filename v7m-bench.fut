-- ==
-- random compiled input { [16384][16]f32  [16384][16]f32 }
-- random compiled input { [32768][16]f32  [32768][16]f32 }
-- random compiled input { [65532][16]f32  [65532][16]f32 }
-- random compiled input { [131072][16]f32  [131072][16]f32 }
-- random compiled input { [262144][16]f32  [262144][16]f32 }
-- random compiled input { [524288][16]f32  [524288][16]f32 }
-- random compiled input { [1048576][16]f32 [1048576][16]f32 }


open import "v7m"

entry main [n][m][d] (P: [n][d]f32) (Q: [m][d]f32) =
  v7m GetLeafSizeLb GetK P Q |> (.1)
