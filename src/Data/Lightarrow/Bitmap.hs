{-

We represent 2D image artifacts/assets abstractly with |Bitmap|s.

-}
module Data.Lightarrow.Bitmap where

import Data.Lightarrow.Artifact
{-

We want to know the width and height of bitmaps in pixels. One reason is
proper positioning.

-}
class ArtifactPlatform (Bitmap m) m => BitmapPlatform m where
    data Bitmap m
    dimensions :: Bitmap m -> (Int, Int)
