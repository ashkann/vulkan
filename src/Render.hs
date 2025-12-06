{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Render
  ( Render (..),
    applyObject,
  )
where

import Affine (Affine)
import Vertex (Vertex, applyVert)
import Measure (PixelVec)

class Render obj where
  render :: obj -> [Vertex PixelVec]
  {-# MINIMAL render #-}

applyObject :: (Render obj) => Affine -> obj -> [Vertex v] -- TODO simplify types
applyObject tr obj = applyVert tr <$> render obj