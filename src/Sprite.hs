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
      r = s.region
      va = vert 0 0 (vec r.topLeft.x r.topLeft.y)
      vb = vert w 0 (vec r.bottomRight.x r.topLeft.y)
      vc = vert w h (vec r.bottomRight.x r.bottomRight.y)
      vd = vert 0 h (vec r.topLeft.x r.bottomRight.y)
      vert x y uv = colorVertex (vec x y) uv s.texture (Just s.color)