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
import Measure hiding (transform)
import Vertex (Vertex, applyVert)

class Render obj where
  render :: obj -> [Vertex PixelVec]
  {-# MINIMAL render #-}

instance (Render a) => Render [a] where
  render :: [a] -> [Vertex PixelVec]
  render = (render =<<)

applyObject :: (Render obj, Vec v, Element v ~ Float) => Affine -> obj -> [Vertex v] -- TODO simplify types
applyObject m obj = applyVert m <$> render obj