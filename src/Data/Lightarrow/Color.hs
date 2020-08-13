module Data.Lightarrow.Color where

import Data.Word
{-

We define a type for generic colors with red, green, blue, and alpha channels,
as well as some convenient transformations and constants.

-}
data Color  =  Black
            |  Blue
            |  Color Double Double Double Double
            |  ColorB Word8 Word8 Word8 Word8
            |  Cyan
            |  Dark Color
            |  Gray
            |  Green
            |  Light Color
            |  Magenta
            |  Red
            |  Translucent Color
            |  Viridian
            |  White
            |  Yellow
    deriving (Eq, Ord, Read, Show)
