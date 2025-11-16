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
    applyXY,
    inverse,
    noScale,
    scaleXY,
    noRotation,
    rotate,
    rotateDegree,
    Scale,
    Rotation,
    Tr (..),
    srt,
    sr,
    origin,
    translate,
    scale,
    applyVec,
    translateX,
  )
where

import Measure (NDCVec, PixelVec, Vec (Element, neg, vec), WorldVec, unvec, pattern WithVec)

-- TODO Input i, Output o => Affine i o
-- TODO
data Affine = Affine
  -- X  Y  | T
  { xx, yx, tx :: Float,
    xy, yy, ty :: Float
  }

class (Vec u, Vec v, Element v ~ Float, Element u ~ Float) => Tr u v where
  tr :: Affine -> u -> v
  tr m (WithVec x y) = trXY @u m x y
  trXY :: Affine -> Element u -> Element u -> v
  trXY m x y = let (x', y') = Affine.applyXY m (x, y) in vec x' y'

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

applyXY :: Affine -> (Float, Float) -> (Float, Float)
applyXY Affine {xx, xy, yx, yy, tx, ty} (x, y) = (x', y')
  where
    x' = xx * x + yx * y + tx
    y' = xy * x + yy * y + ty

applyVec :: (Vec u, Vec v, Element u ~ Float, Element v ~ Float) => Affine -> u -> v
applyVec m v = let (x', y') = applyXY m (unvec v) in vec x' y'

srt3 :: (Float, Float) -> Float -> (Float, Float) -> Affine
srt3 s r t = srt4 s r t (0, 0)

srt4 :: (Float, Float) -> Float -> (Float, Float) -> (Float, Float) -> Affine
srt4 (sx, sy) r (tx, ty) (ox, oy) =
  Affine
    { xx = _xx,
      xy = _xy,
      yx = _yx,
      yy = _yy,
      tx = tx - (_xx * ox + _yx * oy),
      ty = ty - (_xy * ox + _yy * oy)
    }
  where
    _xx = sx * c
    _xy = sx * s
    _yx = -(sy * s)
    _yy = sy * c
    c = cos r
    s = sin r

srt :: (Vec p, Vec o, Element p ~ Float, Element o ~ Float) => Scale -> Rotation -> p -> o -> Affine
srt (Scale (sx, sy)) (Rotation r) (WithVec tx ty) (WithVec ox oy) = srt4 (sx, sy) r (tx, ty) (ox, oy)

sr :: Scale -> Rotation -> Affine
sr (Scale (sx, sy)) (Rotation r) = srt3 (sx, sy) r (0, 0)

instance Semigroup Affine where
  a <> b =
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

noRotation :: Rotation
noRotation = rotate 0

rotate :: Float -> Rotation
rotate = Rotation

rotateDegree :: Float -> Rotation
rotateDegree r = rotate $ r * (pi / 180)

translate :: (Vec p, Element p ~ Float) => p -> Affine
translate p = srt noScale noRotation p (vec 0 0 :: PixelVec)

translateX :: Float -> Affine
translateX x = srt3 (1, 1) 0 (x, 0)

origin :: (Vec v, Element v ~ Float) => v -> Affine
origin o = translate (neg o)

scale :: Scale -> Affine
scale s = sr s noRotation