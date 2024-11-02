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
    mkNdcPos,
    mkTexturePos,
    mkNDCSize,
    mkNDCPosFromSize,
    mkNDCPosFromPixel,
    mkPixelPos,
    mkPixelSize,
    mkTextureSize,
    mkNDCSizeFromPixel,
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

mkPixelPos :: Word32 -> Word32 -> PixelPosition
mkPixelPos x y = PixelPosition $ G.uvec2 x y

{-# COMPLETE PixelXY #-}

pattern PixelXY :: Word32 -> Word32 -> PixelPosition
pattern PixelXY x y <- PixelPosition (G.WithUVec2 x y)

newtype PixelSize = PixelSize G.UVec2 deriving (Show)

mkPixelSize :: Word32 -> Word32 -> PixelSize
mkPixelSize w h = PixelSize $ G.uvec2 w h

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

mkNdcPos :: Float -> Float -> NormalizedDevicePosition
mkNdcPos x y = NormalizedDevicePosition $ G.vec2 x y

newtype NormalizedDeviceSize = NormalizedDeviceSize G.Vec2 deriving (Show)

{-# COMPLETE NormalizedDeviceWH #-}

pattern NormalizedDeviceWH :: Float -> Float -> NormalizedDeviceSize
pattern NormalizedDeviceWH w h <- NormalizedDeviceSize (G.WithVec2 w h)

mkNDCSize :: Float -> Float -> NormalizedDeviceSize
mkNDCSize w h = NormalizedDeviceSize $ G.vec2 w h

mkNDCPosFromSize :: NormalizedDeviceSize -> NormalizedDevicePosition
mkNDCPosFromSize (NormalizedDeviceWH w h) = mkNdcPos (w - 1.0) (h - 1.0)

mkNDCPosFromPixel :: PixelPosition -> PixelSize -> NormalizedDevicePosition
mkNDCPosFromPixel (PixelXY x y) (PixelWH w h) =
  let nx = fromIntegral x / fromIntegral w
      ny = fromIntegral y / fromIntegral h
   in mkNdcPos (nx - 1.0) (ny - 1.0)

mkNDCSizeFromPixel :: PixelSize -> PixelSize -> NormalizedDeviceSize
mkNDCSizeFromPixel (PixelWH w h) (PixelWH sw sh) =
  let nw = fromIntegral w / fromIntegral sw
      nh = fromIntegral h / fromIntegral sh
   in mkNDCSize (nw * 2.0) (nh * 2.0)

newtype TexturePosition = TexturePosition G.Vec2 deriving (Show)

{-# COMPLETE TextureXY #-}

pattern TextureXY :: Float -> Float -> TexturePosition
pattern TextureXY x y <- TexturePosition (G.WithVec2 x y)

mkTexturePos :: Float -> Float -> TexturePosition
mkTexturePos x y = TexturePosition $ G.vec2 x y

newtype TextureSize = TextureSize G.Vec2 deriving (Show)

mkTextureSize :: Float -> Float -> TextureSize
mkTextureSize w h = TextureSize $ G.vec2 w h

{-# COMPLETE TextureWH #-}

pattern TextureWH :: Float -> Float -> TextureSize
pattern TextureWH w h <- TextureSize (G.WithVec2 w h)

-- data Sapce = World | Device | Model | Texture

-- data CoordinateSystem a = CoordinateSystem
--   { top :: a,
--     left :: a,
--     bottom :: a,
--     right :: a
--   }

-- deviceNormalizedCoordinateSystem :: CoordinateSystem Float
-- deviceNormalizedCoordinateSystem = CoordinateSystem 1.0 (-1.0) (-1.0) 1.0

-- textureCoordinateSystem :: CoordinateSystem Float
-- textureCoordinateSystem = CoordinateSystem 1.0 0.0 0.0 1.0

newtype QuadCorner = QuadCorner (NormalizedDevicePosition, TexturePosition)

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
        ( QuadCorner (mkNdcPos x1 y1, mkTexturePos u1 v1),
          QuadCorner (mkNdcPos x2 y1, mkTexturePos u2 v1),
          QuadCorner (mkNdcPos x2 y2, mkTexturePos u2 v2),
          QuadCorner (mkNdcPos x1 y2, mkTexturePos u1 v2)
        )
  where
    toNdc w u1 u2 x x' =
      let rw = w * (u2 - u1)
          x1 = x' - rw * x
       in (x1, x1 + rw)

ndcTopLeft :: NormalizedDevicePosition
ndcTopLeft = mkNdcPos (-1.0) (-1.0)

ndcTopRight :: NormalizedDevicePosition
ndcTopRight = mkNdcPos 1.0 (-1.0)

ndcBottomLeft :: NormalizedDevicePosition
ndcBottomLeft = mkNdcPos (-1.0) 1.0

ndcBottomRight :: NormalizedDevicePosition
ndcBottomRight = mkNdcPos 1.0 1.0

ndcCenter :: NormalizedDevicePosition
ndcCenter = mkNdcPos 0.0 0.0

texTopLeft :: TexturePosition
texTopLeft = mkTexturePos 0.0 0.0

texTopRight :: TexturePosition
texTopRight = mkTexturePos 1.0 0.0

texBottomLeft :: TexturePosition
texBottomLeft = mkTexturePos 0.0 1.0

texBottomRight :: TexturePosition
texBottomRight = mkTexturePos 1.0 1.0

texCenter :: TexturePosition
texCenter = mkTexturePos 0.5 0.5