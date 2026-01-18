{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Render
  ( Render (..),
    applyObject,
  )
where

import Affine (Affine)
import Measure (PixelVec)
import Vertex (Vertex, applyVert)

class Render obj where
  render :: obj -> [Vertex PixelVec]
  {-# MINIMAL render #-}

applyObject :: (Render obj) => Affine -> obj -> [Vertex v] -- TODO simplify types
applyObject tr obj = applyVert tr <$> render obj