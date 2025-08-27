{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Sprite
  ( Sprite (..),
    In (..),
    putIn,
    rotateSprite,
    bottomLeft,
    projection,
    rotate,
    rotateDegree,
    noRatation,
    screen,
    world,
    scaleXY,
    setRotation,
    setScale,
  )
where

import Affine (Affine, srt, srt2affine)
import Camera (Camera, view)
import qualified Data.Vector.Storable as SV
import Measure
import Render (Render (..))
import Texture
import Vertex (Color, colorVertex)

data Sprite = Sprite
  { texture :: DescriptorIndex,
    region :: UVRegion,
    resolution :: PixelVec,
    origin :: PixelVec
  }

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

rotateSprite :: In WorldVec Sprite -> Rotation -> In WorldVec Sprite
rotateSprite s r = s {rotation = r}

putIn :: obj -> vec -> In obj vec
putIn obj pos = In {object = obj, position = pos, rotation = noRatation, scale = noScale}

setRotation :: Rotation -> In obj vec -> In obj vec
setRotation r s = s {Sprite.rotation = r}

setScale :: Scale -> In obj vec -> In obj vec
setScale k s = s {Sprite.scale = k}

instance (Render.Render (Affine, a) obj) => Render.Render (Camera, PPU, ViewportSize, a) (In obj WorldVec) where
  render (cam, ppu, vps, a) In {object, position = WithVec x0 y0, scale, rotation} = Render.render (w, a) object
    where
      w = world (cam, ppu, vps) (scale, rotation, vec x0 y0) (vec 0 0)

instance Render ViewportSize (In Sprite PixelVec) where
  render vps sis = render (screen vps (sis.scale, sis.rotation, sis.position) sis.object.origin, Nothing) sis.object


screen :: ViewportSize -> (Scale, Rotation, PixelVec) -> PixelVec -> Affine
screen vps (scale, rotation, position) origin = projection vps <> model
  where
    projection (WithVec w h) = srt2affine $ srt (2 / fromIntegral w, 2 / fromIntegral h) 0 (-1, -1)
    model =
      let Scale sx sy = scale
          WithVec x y = position
          WithVec ox oy = origin
          pivot = srt2affine $ srt (1, 1) 0 (-ox, -oy)
          local = srt2affine $ srt (sx, sy) rotation.r (x, y)
       in local <> pivot

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

instance Render (Affine, Maybe Color) Sprite where
  render (tr, clr) s = SV.fromList [va, vb, vc, vc, vd, va]
    where
      WithVec w h = s.resolution
      UVReg2 a b c d = s.region
      va = vert (0, 0) a
      vb = vert (w, 0) b
      vc = vert (w, h) c
      vd = vert (0, h) d
      vert (x, y) uv = colorVertex (tr2 @PixelVec tr x y) uv s.texture clr

projection :: ViewportSize -> PPU -> Affine
projection (WithVec w h) (PPU ppu) = srt2affine $ srt (s w, -(s h)) 0 (0, 0)
  where
    s x = (2 * ppu) / fromIntegral x