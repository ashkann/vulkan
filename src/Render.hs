{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
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

import Affine hiding (origin)
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

scaleFlipY :: Scale -> Scale
scaleFlipY Scale {x = sx, y = sy} = Scale {x = sx, y = -sy}

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

instance (Render obj, Vec vec, Element vec ~ Float) => Render (In obj vec) where
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

world3 :: (Camera, PPU, ViewportSize) -> (Scale, Rotation, WorldVec) -> PixelVec -> Affine
world3 (cam, ppu@(PPU _ppu), vps) (scale, rotation, position) origin = projection vps ppu <> model
  where
    model =
      let s = 1 / _ppu
          Scale sx sy = scale
          WithVec x y = position
          pivot = srt (1, 1) 0 (-ox, -oy)
          local = srt (s * sx, -(s * sy)) rotation.r (x, y) -- Place in world
          WithVec ox oy = origin
       in view cam <> local <> pivot

world :: Camera -> PPU -> ViewportSize -> Affine
world cam ppu vps = projection vps ppu <> view cam

screen :: ViewportSize -> Affine
screen (WithVec w h) = srt (s w, s h) 0 (-1, -1)
  where
    s x = 2 / fromIntegral x

projection :: ViewportSize -> PPU -> Affine
projection (WithVec w h) (PPU ppu) =  srt (s w, -(s h)) 0 (0, 0)
  where
    s x = (2 * ppu) / fromIntegral x