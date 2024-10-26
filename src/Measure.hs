{-# LANGUAGE PatternSynonyms #-}

module Measure
  ( TextureRegion (..),
    PixelPosition (..),
    PixelSize (..),
    pattern PixelXY,
    pattern PixelWH,
  )
where

import Data.Word (Word32)
import qualified Geomancy as G
import Prelude hiding (lookup)

newtype TextureRegion = TextureRegion G.Vec4 deriving (Show)

newtype PixelPosition = PixelPosition G.UVec2 deriving (Show)

{-# COMPLETE PixelXY #-}

pattern PixelXY :: Word32 -> Word32 -> PixelPosition
pattern PixelXY x y <- PixelPosition (G.WithUVec2 x y)

newtype PixelSize = PixelSize G.UVec2 deriving (Show)

{-# COMPLETE PixelWH #-}

pattern PixelWH :: Word32 -> Word32 -> PixelSize
pattern PixelWH w h <- PixelSize (G.WithUVec2 w h)