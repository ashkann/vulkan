{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Sprite
  ( Sprite (..),
  )
where

import Measure (PixelVec, Region(..), UV, Vec (..), vec)
import Render
import Texture
import Vertex (Color, colorVertex)

data Sprite = Sprite
  { texture :: DescriptorIndex,
    region :: Region UV,
    resolution :: PixelVec,
    color :: Color
  }

instance Render Sprite where
  render s = [a, b, c, c, d, a]
    where
      Vec w h = s.resolution
      r = s.region
      a = vert (0, 0) (r.topLeft.x, r.topLeft.y)
      b = vert (w, 0) (r.bottomRight.x, r.topLeft.y)
      c = vert (w, h) (r.bottomRight.x, r.bottomRight.y)
      d = vert (0, h) (r.topLeft.x, r.bottomRight.y)
      vert (x, y) (u, v) = colorVertex (vec x y) (vec u v) s.texture (Just s.color)