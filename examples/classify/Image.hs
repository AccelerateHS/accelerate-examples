module Image where

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar                     ( Array(..) )
import Prelude                                               hiding ( map, fromIntegral )

inputFromImage :: Acc (Array DIM2 Word8) -> Acc (Array DIM2 Float)
inputFromImage = map ((/256.0) . fromIntegral)

-- Yield a sequence containing all the loaded images.
--
-- There is some ugly hackery as we don't yet support subarrays on 3D arrays
-- yet. TODO: fix this
--
allImages :: Array DIM3 Word8 -> Seq [Vector Float]
allImages arr@(Array ((((),c),h),w) input) =
    mapSeq (flatten . inputFromImage)
  $ subarrays (lift (Z:.h:.w)) (Array (((),h*c),w) input)
