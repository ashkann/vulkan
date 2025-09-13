{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Affine
  ( Affine,
    srt3,
    apply,
    inverse,
    scale,
    noScale,
    scaleXY,
    noRatation,
    rotateDegree,
    Scale,
    Rotation,
    Tr (..),
    srt,
    sr,
    origin,
    translate,
  )
where

import Measure (NDCVec, PixelVec, Vec (Element, neg, vec), WorldVec, pattern WithVec)

-- TODO Input i, Output o => Affine i o
-- TODO
data Affine = Affine
  -- X  Y  | T
  { xx, yx, tx :: Float,
    xy, yy, ty :: Float
  }

class (Vec u, Vec v, Element v ~ Float, Element u ~ Float) => Tr u v where
  tr :: Affine -> u -> v
  tr m (WithVec x y) = tr2 @u m x y
  tr2 :: Affine -> Element u -> Element u -> v
  tr2 m x y = let (x', y') = Affine.apply m (x, y) in vec x' y'

instance Tr NDCVec NDCVec

instance Tr WorldVec NDCVec

instance Tr WorldVec WorldVec

instance Tr PixelVec WorldVec

instance Tr PixelVec NDCVec

inverse :: Affine -> Affine
inverse m =
  Affine
    { xx = xx,
      xy = xy,
      yx = yx,
      yy = yy,
      tx = -(xx * m.tx + xy * m.ty),
      ty = -(yx * m.tx + yy * m.ty)
    }
  where
    invDet = 1 / (m.xx * m.yy - m.xy * m.yx)
    xx = m.yy * invDet
    xy = -(m.xy * invDet)
    yx = -(m.yx * invDet)
    yy = m.xx * invDet

apply :: Affine -> (Float, Float) -> (Float, Float)
apply Affine {xx, xy, yx, yy, tx, ty} (x, y) = (x', y')
  where
    x' = xx * x + yx * y + tx
    y' = xy * x + yy * y + ty

srt3 :: (Float, Float) -> Float -> (Float, Float) -> Affine
srt3 (sx, sy) r (tx, ty) =
  Affine
    { xx = sx * c,
      xy = sx * s,
      yx = -(sy * s),
      yy = sy * c,
      tx = tx,
      ty = ty
    }
  where
    c = cos r
    s = sin r

srt :: (Vec v, Element v ~ Float) => Scale -> Rotation -> v -> Affine
srt (Scale (sx, sy)) (Rotation r) (WithVec tx ty) = srt3 (sx, sy) r (tx, ty)

sr :: Scale -> Rotation -> Affine
sr (Scale (sx, sy)) (Rotation r) = srt3 (sx, sy) r (0, 0)

instance Semigroup Affine where
  (<>) a b =
    Affine
      { xx = a.xx * b.xx + a.yx * b.xy,
        xy = a.xy * b.xx + a.yy * b.xy,
        yx = a.xx * b.yx + a.yx * b.yy,
        yy = a.xy * b.yx + a.yy * b.yy,
        tx = a.xx * b.tx + a.yx * b.ty + a.tx,
        ty = a.xy * b.tx + a.yy * b.ty + a.ty
      }

instance Monoid Affine where
  mempty = Affine {xx = 1, xy = 0, yx = 0, yy = 1, tx = 0, ty = 0}

newtype Scale = Scale (Float, Float)

noScale :: Scale
noScale = Scale (1.0, 1.0)

scaleXY :: Float -> Float -> Scale
scaleXY sx sy = Scale (sx, sy)

newtype Rotation = Rotation Float

noRatation :: Rotation
noRatation = rotate 0

rotate :: Float -> Rotation
rotate = Rotation

rotateDegree :: Float -> Rotation
rotateDegree r = rotate $ r * (2 * pi / 360)

translate :: (Vec v, Element v ~ Float) => v -> Affine
translate = srt noScale noRatation

origin :: (Vec v, Element v ~ Float) => v -> Affine
origin o = translate (neg o)

scale :: Float -> Float -> Affine
scale sx sy = srt3 (sx, sy) 0 (0, 0)