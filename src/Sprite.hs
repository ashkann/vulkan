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
  renderColored)
where

import Affine (Affine)
import qualified Data.Vector.Storable as SV
import Measure
import Render
import Texture
import Vertex (Color, Vertex, colorVertex)

data Sprite = Sprite
  { texture :: DescriptorIndex,
    region :: UVRegion,
    resolution :: PixelVec,
    color :: Color
  }

instance Render Sprite where
  render s tr = SV.fromList [va, vb, vc, vc, vd, va]
    where
      WithVec w h = s.resolution
      UVReg2 a b c d = s.region
      va = vert (0, 0) a
      vb = vert (w, 0) b
      vc = vert (w, h) c
      vd = vert (0, h) d
      vert (x, y) uv = colorVertex (tr2 @PixelVec tr x y) uv s.texture (Just s.color)

renderColored :: Sprite -> Color -> Affine -> SV.Vector Vertex
renderColored s c = render (s {color = c})