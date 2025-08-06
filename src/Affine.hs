{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Affine
  ( SRT,
    Affine,
    srt,
    apply,
    affine,
    srt2affine,
  )
where

data SRT = SRT
  { sx, sy :: Float,
    r :: Float,
    tx, ty :: Float
  }

srt :: (Float, Float) -> Float -> (Float, Float) -> SRT
srt (sx, sy) r (tx, ty) = SRT sx sy r tx ty

data Affine = Affine
  -- X  Y  | T
  { xx, yx, tx :: Float,
    xy, yy, ty :: Float
  }

apply :: Affine -> (Float, Float) -> (Float, Float)
apply Affine {xx, xy, yx, yy, tx, ty} (x, y) = (x', y')
  where
    x' = xx * x + yx * y + tx
    y' = xy * x + yy * y + ty

affine :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Affine
affine (xx, xy) (yx, yy) (tx, ty) = Affine {xx, xy, yx, yy, tx, ty}

srt2affine :: SRT -> Affine
srt2affine (SRT {sx, sy, r, tx, ty}) =
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