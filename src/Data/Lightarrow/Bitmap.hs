module Data.Lightarrow.Bitmap where

import Data.Lightarrow.Artifact

-- | Platforms that represent 2D raster images
class ArtifactPlatform (Bitmap m) m => BitmapPlatform m where
    -- | The type of the image representation
    data Bitmap m
    -- | The width and height of a bitmap in pixels
    dimensions :: Bitmap m -> (Int, Int)