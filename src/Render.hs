{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Render where

import qualified Data.Vector.Storable as SV
import Vertex (Vertex)

class Render a obj | obj -> a where
  render :: a -> obj -> SV.Vector Vertex -- TODO any Traversable would do ?