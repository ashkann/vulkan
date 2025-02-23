{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module Measure
  ( PixelPosition (..),
    PixelSize (..),
    PixelRegion (..),
    UVRegion (..),
    TexturePosition,
    TextureSize,
    NormalizedDevicePosition,
    NormalizedDeviceSize,
    ndcPos,
    uvPos,
    ndcSize,
    pixelPosToNdc,
    pixelPos,
    pixelSize,
    uvSize,
    pixelSizeToNdc,
    pattern TextureXY,
    pattern NormalizedDeviceWH,
    pattern NormalizedDeviceXY,
    pattern PixelXY,
    pattern PixelWH,
    pattern PixelReg,
    pattern UVReg,
    ndcTopLeft,
    ndcTopRight,
    ndcBottomLeft,
    ndcBottomRight,
    ndcCenter,
    texTopLeft,
    texTopRight,
    texBottomLeft,
    texBottomRight,
    texCenter,
    ndcVec,
    texVec,
    pixelPosToTex,
    pixelReg,
    uvReg,
    localToNdc,
    localPosToNdc,
    ndcTranslate,
    translate,
    transform
  )
where

import Data.Word (Word32)
import qualified Geomancy as G
import Prelude hiding (lookup)
import qualified Geomancy.Transform as G

-- data TextureRegion = TextureRegion {uv :: G.Vec4, pixels :: G.UVec4} deriving (Show)

-- {-# COMPLETE TextureRegionXY #-}

-- pattern TextureRegionXY :: Float -> Float -> Float -> Float -> TextureRegion
-- pattern TextureRegionXY u1 v1 u2 v2 <- TextureRegion (G.WithVec4 u1 v1 u2 v2)

newtype PixelPosition = PixelPosition G.UVec2 deriving (Show)

pixelPos :: Word32 -> Word32 -> PixelPosition
pixelPos x y = PixelPosition $ G.uvec2 x y

{-# COMPLETE PixelXY #-}

pattern PixelXY :: Word32 -> Word32 -> PixelPosition
pattern PixelXY x y <- PixelPosition (G.WithUVec2 x y)

newtype PixelSize = PixelSize G.UVec2 deriving (Show)

pixelSize :: Word32 -> Word32 -> PixelSize
pixelSize w h = PixelSize $ G.uvec2 w h

{-# COMPLETE PixelWH #-}

pattern PixelWH :: Word32 -> Word32 -> PixelSize
pattern PixelWH w h <- PixelSize (G.WithUVec2 w h)

newtype PixelRegion = PixelRegion G.UVec4 deriving (Show)

pixelReg :: Word32 -> Word32 -> Word32 -> Word32 -> PixelRegion
pixelReg x1 y1 x2 y2 = PixelRegion $ G.uvec4 x1 y1 x2 y2

{-# COMPLETE PixelReg #-}

pattern PixelReg :: Word32 -> Word32 -> Word32 -> Word32 -> PixelRegion -- TODO: make it bidirectional
pattern PixelReg x1 y1 x2 y2 <- PixelRegion (G.WithUVec4 x1 y1 x2 y2)

newtype UVRegion = UVRegion G.Vec4 deriving (Show)

uvReg :: Float -> Float -> Float -> Float -> UVRegion
uvReg u1 v1 u2 v2 = UVRegion $ G.vec4 u1 v1 u2 v2

{-# COMPLETE UVReg #-}

pattern UVReg :: Float -> Float -> Float -> Float -> UVRegion -- TODO: make it bidirectional
pattern UVReg u1 v1 u2 v2 <- UVRegion (G.WithVec4 u1 v1 u2 v2)

class Transform a where
  transform :: a -> G.Transform

newtype NormalizedDevicePosition = NormalizedDevicePosition G.Vec2 deriving (Show)

instance Transform NormalizedDevicePosition where
  transform (NormalizedDeviceXY x y) = G.translate x y 1

{-# COMPLETE NormalizedDeviceXY #-}

ndcVec :: NormalizedDevicePosition -> G.Vec2
ndcVec (NormalizedDevicePosition v) = v

{-# COMPLETE NormalizedDeviceXY #-}

pattern NormalizedDeviceXY :: Float -> Float -> NormalizedDevicePosition
pattern NormalizedDeviceXY x y <- NormalizedDevicePosition (G.WithVec2 x y)

class Translate a b where
  translate :: a -> b -> b

instance Translate G.Vec2 NormalizedDevicePosition where
  translate d (NormalizedDevicePosition p) = NormalizedDevicePosition $ p + d

instance Translate NormalizedDeviceSize NormalizedDevicePosition where
  translate (NormalizedDeviceSize s) = translate s

instance Translate NormalizedDevicePosition NormalizedDevicePosition where
  translate (NormalizedDevicePosition a) (NormalizedDevicePosition b) = NormalizedDevicePosition $ a + b

ndcTranslate :: G.Vec2 -> NormalizedDevicePosition -> NormalizedDevicePosition
ndcTranslate d (NormalizedDevicePosition p) = translate d (NormalizedDevicePosition p)

texVec :: TexturePosition -> G.Vec2
texVec (TexturePosition v) = v

pixelPosToTex :: PixelSize -> PixelPosition -> TexturePosition
pixelPosToTex (PixelWH w h) (PixelXY x y) =
  let nx = fromIntegral x / fromIntegral w
      ny = fromIntegral y / fromIntegral h
   in uvPos nx ny

ndcPos :: Float -> Float -> NormalizedDevicePosition
ndcPos x y = NormalizedDevicePosition $ G.vec2 x y

newtype NormalizedDeviceSize = NormalizedDeviceSize G.Vec2 deriving (Show)

{-# COMPLETE NormalizedDeviceWH #-}

pattern NormalizedDeviceWH :: Float -> Float -> NormalizedDeviceSize
pattern NormalizedDeviceWH w h <- NormalizedDeviceSize (G.WithVec2 w h)

ndcSize :: Float -> Float -> NormalizedDeviceSize
ndcSize w h = NormalizedDeviceSize $ G.vec2 w h

pixelPosToNdc :: PixelPosition -> PixelSize -> NormalizedDevicePosition
pixelPosToNdc (PixelXY x y) (PixelWH w h) =
  let nx = (fromIntegral x / fromIntegral w) * 2
      ny = (fromIntegral y / fromIntegral h) * 2
   in ndcPos (nx - 1.0) (ny - 1.0)

pixelSizeToNdc :: PixelSize -> PixelSize -> NormalizedDeviceSize
pixelSizeToNdc (PixelWH w h) (PixelWH windowWidth windowHeight) =
  let nw = fromIntegral w / fromIntegral windowWidth
      nh = fromIntegral h / fromIntegral windowHeight
   in ndcSize (nw * 2.0) (nh * 2.0)

newtype TexturePosition = TexturePosition G.Vec2 deriving (Show)

{-# COMPLETE TextureXY #-}

pattern TextureXY :: Float -> Float -> TexturePosition
pattern TextureXY x y <- TexturePosition (G.WithVec2 x y)

uvPos :: Float -> Float -> TexturePosition
uvPos x y = TexturePosition $ G.vec2 x y

newtype TextureSize = TextureSize G.Vec2 deriving (Show)

uvSize :: Float -> Float -> TextureSize
uvSize w h = TextureSize $ G.vec2 w h

localPosToNdc :: NormalizedDeviceSize -> TexturePosition -> NormalizedDevicePosition -> NormalizedDevicePosition
localPosToNdc (NormalizedDeviceWH w h) (TextureXY ox oy) (NormalizedDeviceXY x y) =
  let x1 = localToNdc w ox x
      y1 = localToNdc h oy y
   in ndcPos x1 y1

localToNdc :: Float -> Float -> Float -> Float
localToNdc ndcWidth factor x = x - ndcWidth * factor

ndcTopLeft :: NormalizedDevicePosition
ndcTopLeft = ndcPos (-1.0) (-1.0)

ndcTopRight :: NormalizedDevicePosition
ndcTopRight = ndcPos 1.0 (-1.0)

ndcBottomLeft :: NormalizedDevicePosition
ndcBottomLeft = ndcPos (-1.0) 1.0

ndcBottomRight :: NormalizedDevicePosition
ndcBottomRight = ndcPos 1.0 1.0

ndcCenter :: NormalizedDevicePosition
ndcCenter = ndcPos 0.0 0.0

texTopLeft :: TexturePosition
texTopLeft = uvPos 0.0 0.0

texTopRight :: TexturePosition
texTopRight = uvPos 1.0 0.0

texBottomLeft :: TexturePosition
texBottomLeft = uvPos 0.0 1.0

texBottomRight :: TexturePosition
texBottomRight = uvPos 1.0 1.0

texCenter :: TexturePosition
texCenter = uvPos 0.5 0.5