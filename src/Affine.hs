{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Affine
  ( Affine,
    srt3,
    srt4,
    inverse,
    noScale,
    scaleXY,
    noRotation,
    rotate,
    rotateDegree,
    Scale,
    Rotation,
    srt,
    sr,
    origin,
    scale,
    applyVec,
    translateX,
    translateXY,
    uniformScale,
    originXY,
    translate,
    combine,
    aff1,
    noTranslation,
  )
where

import Measure (Vec (..), vec)

data Affine u v = Affine
  -- X  Y  | T
  { xx, yx, tx :: Float,
    xy, yy, ty :: Float
  }

inverse :: Affine u v -> Affine v u
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

applyVec :: Affine u v -> Vec u -> Vec v
applyVec (Affine {xx, xy, yx, yy, tx, ty}) (Vec x y) = vec x' y'
  where
    x' = xx * x + yx * y + tx
    y' = xy * x + yy * y + ty

-- TODO: remove, unsafe
srt3 :: (Float, Float) -> Float -> (Float, Float) -> Affine u v
srt3 s r t = srt4 s r t (0, 0)

-- TODO: remove, unsafe

-- | `t` and `o` are in world units
srt4 ::
  -- | Scale x and y
  (Float, Float) ->
  -- | Rotatoin in gradiants, positive is from +x to +y
  Float ->
  -- | Translation x and y
  (Float, Float) ->
  -- | Origin x and y
  (Float, Float) ->
  Affine u v
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

-- TODO: there must be a relation between t o u v
srt :: Scale -> Rotation -> Vec t -> Vec o -> Affine u v
srt (Scale (sx, sy)) (Rotation r) (Vec tx ty) (Vec ox oy) = srt4 (sx, sy) r (tx, ty) (ox, oy)

sr :: Scale -> Rotation -> Affine u v
sr (Scale (sx, sy)) (Rotation r) = srt3 (sx, sy) r (0, 0)

combine :: Affine a v -> Affine u a -> Affine u v
a `combine` b =
  Affine
    { xx = a.xx * b.xx + a.yx * b.xy,
      xy = a.xy * b.xx + a.yy * b.xy,
      yx = a.xx * b.yx + a.yx * b.yy,
      yy = a.xy * b.yx + a.yy * b.yy,
      tx = a.xx * b.tx + a.yx * b.ty + a.tx,
      ty = a.xy * b.tx + a.yy * b.ty + a.ty
    }

aff1 :: Affine u u
aff1 = Affine {xx = 1, xy = 0, yx = 0, yy = 1, tx = 0, ty = 0}

newtype Scale = Scale (Float, Float)

noScale :: Scale
noScale = Scale (1.0, 1.0)

scaleXY :: Float -> Float -> Scale
scaleXY sx sy = Scale (sx, sy)

uniformScale :: Float -> Scale
uniformScale s = scaleXY s s

newtype Rotation = Rotation Float

noRotation :: Rotation
noRotation = rotate 0

noTranslation :: Affine v v
noTranslation = translateXY 0 0

rotate :: Float -> Rotation
rotate = Rotation

rotateDegree :: Float -> Rotation
rotateDegree r = rotate $ r * (pi / 180)

translate :: Vec v -> Affine v v
translate p = srt noScale noRotation p (vec 0 0)

translateXY :: Float -> Float -> Affine v v
translateXY x y = srt3 (1, 1) 0 (x, y)

translateX :: Float -> Affine v v
translateX x = srt3 (1, 1) 0 (x, 0)

origin :: Vec o -> Affine v v
origin (Vec ox oy) = translateXY (-ox) (-oy)

originXY :: Float -> Float -> Affine v v
originXY ox oy = translateXY (-ox) (-oy)

scale :: Scale -> Affine v v
scale s = sr s noRotation