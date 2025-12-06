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

import Measure
import Render
import Texture
import Vertex (Color, colorVertex)

data Sprite = Sprite
  { texture :: DescriptorIndex,
    region :: UVRegion,
    resolution :: PixelVec,
    color :: Color
  }

instance Render Sprite where
  render s = [va, vb, vc, vc, vd, va]
    where
      Vec w h = s.resolution
      va = vert 0 0 (vec s.region.x1 s.region.y1)
      vb = vert w 0 (vec s.region.x2 s.region.y1)
      vc = vert w h (vec s.region.x2 s.region.y2)
      vd = vert 0 h (vec s.region.x1 s.region.y2)
      vert x y uv = colorVertex (vec x y) uv s.texture (Just s.color)