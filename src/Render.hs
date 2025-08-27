{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
    projection
  )
where

import Affine
import Camera (Camera, view)
import qualified Data.Vector.Storable as SV
import Measure
import Vertex (Vertex)

class Render a obj | obj -> a where
  render :: a -> obj -> SV.Vector Vertex -- TODO any Traversable would do ?

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

data In obj vec = In {object :: obj, position :: vec, rotation :: Rotation, scale :: Scale}

putIn :: obj -> vec -> In obj vec
putIn obj pos = In {object = obj, position = pos, rotation = noRatation, scale = noScale}

setRotation :: Rotation -> In obj vec -> In obj vec
setRotation r s = s {rotation = r}

setScale :: Scale -> In obj vec -> In obj vec
setScale k s = s {scale = k}

instance (Render.Render (Affine, a) obj) => Render.Render (Camera, PPU, ViewportSize, a) (In obj WorldVec) where
  render (cam, ppu, vps, a) In {object, position = WithVec x0 y0, scale, rotation} = Render.render (w, a) object
    where
      w = world (cam, ppu, vps) (scale, rotation, vec x0 y0) (vec 0 0)

world :: (Camera, PPU, ViewportSize) -> (Scale, Rotation, WorldVec) -> PixelVec -> Affine
world (cam, ppu@(PPU _ppu), vps) (scale, rotation, position) origin = projection vps ppu <> model
  where
    model =
      let s = 1 / _ppu
          Scale sx sy = scale
          WithVec x y = position
          pivot = srt2affine $ srt (1, 1) 0 (-ox, -oy)
          local = srt2affine $ srt (s * sx, -(s * sy)) rotation.r (x, y) -- Place in world
          WithVec ox oy = origin
       in view cam <> local <> pivot

projection :: ViewportSize -> PPU -> Affine
projection (WithVec w h) (PPU ppu) = srt2affine $ srt (s w, -(s h)) 0 (0, 0)
  where
    s x = (2 * ppu) / fromIntegral x