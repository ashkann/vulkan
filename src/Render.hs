{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Render (Render (..)) where

import Vertex (Vertex)

class Render c v where
  render :: c -> [Vertex v]
  {-# MINIMAL render #-}