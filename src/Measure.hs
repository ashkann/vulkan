{-# LANGUAGE PatternSynonyms #-}

module Measure
  ( PixelPosition (..),
    PixelSize (..),
    TextureRegion (..),
    TexturePosition,
    TextureSize,
    NormalizedDevicePosition,
    NormalizedDeviceSize,
    Quad (..),
    QuadCorner (..),
    ndcPos,
    texPos,
    ndcSize,
    pixelPosToNdc,
    pixelPos,
    pixelSize,
    texSize,
    pixelSizeToNdc,
    pattern TextureRegionXY,
    pattern TextureXY,
    pattern NormalizedDeviceWH,
    pattern NormalizedDeviceXY,
    pattern PixelXY,
    pattern PixelWH,
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
    localToNdc,
    ndcVec,
    texVec,
    pixelPosToTex,
  )
where

import Data.Word (Word32)
import qualified Geomancy as G
import Prelude hiding (lookup)

newtype TextureRegion = TextureRegion G.Vec4 deriving (Show)

{-# COMPLETE TextureRegionXY #-}

pattern TextureRegionXY :: Float -> Float -> Float -> Float -> TextureRegion
pattern TextureRegionXY u1 v1 u2 v2 <- TextureRegion (G.WithVec4 u1 v1 u2 v2)

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

newtype NormalizedDevicePosition = NormalizedDevicePosition G.Vec2 deriving (Show)

ndcVec :: NormalizedDevicePosition -> G.Vec2
ndcVec (NormalizedDevicePosition v) = v

{-# COMPLETE NormalizedDeviceXY #-}

pattern NormalizedDeviceXY :: Float -> Float -> NormalizedDevicePosition
pattern NormalizedDeviceXY x y <- NormalizedDevicePosition (G.WithVec2 x y)

texVec :: TexturePosition -> G.Vec2
texVec (TexturePosition v) = v

pixelPosToTex :: PixelSize -> PixelPosition -> TexturePosition
pixelPosToTex (PixelWH w h) (PixelXY x y) =
  let nx = fromIntegral x / fromIntegral w
      ny = fromIntegral y / fromIntegral h
   in texPos nx ny

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
pixelSizeToNdc (PixelWH w h) (PixelWH sw sh) =
  let nw = fromIntegral w / fromIntegral sw
      nh = fromIntegral h / fromIntegral sh
   in ndcSize (nw * 2.0) (nh * 2.0)

newtype TexturePosition = TexturePosition G.Vec2 deriving (Show)

{-# COMPLETE TextureXY #-}

pattern TextureXY :: Float -> Float -> TexturePosition
pattern TextureXY x y <- TexturePosition (G.WithVec2 x y)

texPos :: Float -> Float -> TexturePosition
texPos x y = TexturePosition $ G.vec2 x y

newtype TextureSize = TextureSize G.Vec2 deriving (Show)

texSize :: Float -> Float -> TextureSize
texSize w h = TextureSize $ G.vec2 w h

newtype QuadCorner = Corner (NormalizedDevicePosition, TexturePosition)

newtype Quad = Quad (QuadCorner, QuadCorner, QuadCorner, QuadCorner)

-- | Create a quad in the NDC from local coordinates
localToNdc ::
  NormalizedDeviceSize ->
  TextureRegion ->
  TexturePosition ->
  NormalizedDevicePosition ->
  Quad
localToNdc (NormalizedDeviceWH w h) (TextureRegionXY u1 v1 u2 v2) (TextureXY x y) (NormalizedDeviceXY x' y') =
  let (x1, x2) = toNdc w u1 u2 x x'
      (y1, y2) = toNdc h v1 v2 y y'
   in Quad
        ( Corner (ndcPos x1 y1, texPos u1 v1),
          Corner (ndcPos x2 y1, texPos u2 v1),
          Corner (ndcPos x2 y2, texPos u2 v2),
          Corner (ndcPos x1 y2, texPos u1 v2)
        )
  where
    toNdc w u1 u2 x x' =
      let rw = w * (u2 - u1)
          x1 = x' - rw * x
       in (x1, x1 + rw)

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
texTopLeft = texPos 0.0 0.0

texTopRight :: TexturePosition
texTopRight = texPos 1.0 0.0

texBottomLeft :: TexturePosition
texBottomLeft = texPos 0.0 1.0

texBottomRight :: TexturePosition
texBottomRight = texPos 1.0 1.0

texCenter :: TexturePosition
texCenter = texPos 0.5 0.5