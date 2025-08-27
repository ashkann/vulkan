{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    rotate,
    rotateDegree,
    noRatation,
    screen,
    scaleXY,
    setRotation,
    setScale,
  )
where

import Affine (Affine, srt, srt2affine)
import qualified Data.Vector.Storable as SV
import Measure
import Render
import Texture
import Vertex (Color, colorVertex)

data Sprite = Sprite
  { texture :: DescriptorIndex,
    region :: UVRegion,
    resolution :: PixelVec,
    origin :: PixelVec
  }

rotateSprite :: In WorldVec Sprite -> Rotation -> In WorldVec Sprite
rotateSprite s r = s {rotation = r}

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