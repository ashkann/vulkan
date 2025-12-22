{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Measure
  ( PixelVec,
    WorldVec,
    NDCVec,
    UVVec,
    NDC,
    UV,
    Vec (..),
    vec,
    ViewportSize (..),
    mkWindowSize,
    Space,
    Region(..),
    region,
  )
where

import Data.Word (Word32)
import Foreign.Storable (Storable (..), sizeOf)
import qualified Foreign.Storable.Record as Store
import Prelude hiding (lookup)

data Vec a = Vec {x :: Float, y :: Float}

class Space s

vec :: Float -> Float -> Vec s
vec = Vec

instance Num (Vec s) where
  (+) :: Vec s -> Vec s -> Vec s
  (Vec x y) + (Vec a b) = vec (x + y) (a + b)
  negate :: Vec s -> Vec s
  negate (Vec x y) = vec (-x) (-y)

vecStore :: Store.Dictionary (Vec s)
vecStore = Store.run $ Vec <$> Store.element (.x) <*> Store.element (.y)

instance Storable (Vec s) where
  sizeOf = Store.sizeOf vecStore
  alignment = Store.alignment vecStore
  peek = Store.peek vecStore
  poke = Store.poke vecStore

data NDC

-- instance Space NDC
type NDCVec = Vec NDC

data Pixel

-- instance Space Pixel
type PixelVec = Vec Pixel

data World

instance Space World

type WorldVec = Vec World

data UV

type UVVec = Vec UV

instance Space UV

data ViewportSize = ViewportSize Word32 Word32 deriving (Show)

mkWindowSize :: Word32 -> Word32 -> Maybe ViewportSize
mkWindowSize w h
  | w >= 0 && h >= 0 = Just $ ViewportSize w h
  | otherwise = Nothing

data Region s = Region {topLeft :: Vec s, bottomRight :: Vec s}

region :: Vec s -> Vec s -> Region s
region = Region