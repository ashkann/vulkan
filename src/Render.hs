{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Render
  ( Render (..),
    In (..),
    Rotation (..),
    Scale (..),
    rotate,
    rotateDegree,
    noRatation,
    scaleXY,
    noScale,
    putIn,
    setRotation,
    setScale,
    projection,
    world,
    screen,
    setOrigin,
    srtPutIn,
  )
where

import Affine (Affine, srt)
import qualified Affine
import Camera (Camera, view)
import qualified Data.Vector.Storable as SV
import Measure
import Vertex (Vertex)

class Render obj where
  render :: obj -> Affine -> SV.Vector Vertex -- TODO any Traversable would do ?

data Scale = Scale {x :: Float, y :: Float}

noScale :: Scale
noScale = Scale {x = 1.0, y = 1.0}

scaleXY :: Float -> Float -> Scale
scaleXY sx sy = Scale {x = sx, y = sy}

newtype Rotation = Rotation {r :: Float}

noRatation :: Rotation
noRatation = rotate 0

rotate :: Float -> Rotation
rotate = Rotation

rotateDegree :: Float -> Rotation
rotateDegree r = rotate $ r * (2 * pi / 360)

data In obj vec = In {object :: obj, position :: vec, rotation :: Rotation, scale :: Scale, origin :: PixelVec}

putIn :: obj -> vec -> In obj vec
putIn obj pos = In {object = obj, position = pos, rotation = noRatation, scale = noScale, origin = vec 0 0}

srtPutIn :: obj -> Scale -> Rotation -> vec -> PixelVec -> In obj vec
srtPutIn obj s r pos o = In {object = obj, position = pos, rotation = r, scale = s, origin = o}

setRotation :: Rotation -> In obj vec -> In obj vec
setRotation r s = s {rotation = r}

setScale :: Scale -> In obj vec -> In obj vec
setScale k s = s {scale = k}

setOrigin :: PixelVec -> In obj vec -> In obj vec
setOrigin o s = s {origin = o}

instance (Render obj) => Render (In obj WorldVec) where
  render
    In
      { object,
        position = WithVec x y,
        scale = Scale sx sy,
        rotation = Rotation r,
        origin = WithVec ox oy
      }
    tr =
      render object (tr <> putAt <> localToWorld <> local <> pivot)
      where
        pivot = Affine.origin (-ox) (-oy)
        local = srt (sx, sy) r (0, 0)
        localToWorld = withPPU (\ppu -> Affine.scale (1 / ppu) (-(1 / ppu))) $ ppu 100
        putAt = Affine.translate x y

instance (Render obj) => Render (In obj PixelVec) where
  render
    In
      { object,
        position = WithVec x y,
        scale = Scale sx sy,
        rotation = Rotation r,
        origin = WithVec ox oy
      }
    tr =
      render object (tr <> local <> pivot)
      where
        pivot = srt (1, 1) 0 (-ox, -oy)
        local = srt (sx, sy) r (x, y)

world :: Camera -> PPU -> ViewportSize -> Affine
world cam ppu vps = projection vps ppu <> view cam

screen :: ViewportSize -> Affine
screen (WithVec w h) = srt (s w, s h) 0 (-1, -1)
  where
    s x = 2 / fromIntegral x

projection :: ViewportSize -> PPU -> Affine
projection (WithVec w h) ppu = srt (s w, -(s h)) 0 (0, 0)
  where
    s x = withPPU (\ppu -> (2 * ppu) / fromIntegral x) ppu