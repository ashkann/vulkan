{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module SRT
  ( SRT,
    srt,
    inv,
    apply,
  )
where

data SRT = SRT
  { sx :: Float,
    sy :: Float,
    r :: Float,
    tx :: Float,
    ty :: Float
  }

srt :: (Float, Float) -> Float -> (Float, Float) -> SRT
srt (sx, sy) r (tx, ty) = SRT sx sy r tx ty

inv :: SRT -> SRT
inv (SRT {sx, sy, r, tx, ty}) =
  SRT
    { sx = 1 / sx,
      sy = 1 / sy,
      r = -r,
      tx = -(x1 * c) - y1 * s,
      ty = x1 * s - y1 * c
    }
  where
    x1 = tx / sx
    y1 = ty / sy
    c = cos r
    s = sin r

instance Semigroup SRT where
  (<>) a b =
    SRT
      { sx = a.sx * b.sx,
        sy = a.sy * b.sy,
        r = a.r + b.r,
        tx = a.tx + a.sx * b.tx * c - a.sy * b.ty * s,
        ty = a.ty + a.sx * b.tx * s + a.sy * b.ty * c
      }
    where
      c = cos a.r
      s = sin a.r

instance Monoid SRT where
  mempty = SRT {sx = 1.0, sy = 1.0, r = 0, tx = 0.0, ty = 0.0}

apply :: SRT -> Float -> Float -> (Float, Float)
apply (SRT {sx, sy, r, tx, ty}) x y = (x2, y2)
  where
    x2 = sxx * c - syy * s + tx
    y2 = sxx * s + syy * c + ty
    s = sin r
    c = cos r
    sxx = sx * x
    syy = sy * y