{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Measure
  ( PixelVec,
    NDCVec,
    UVVec,
    Vec (..),
    PixelRegion (..),
    UVRegion (..),
    Normalized (..),
    pixelPosToNdc,
    pixelSizeToNdc,
    pattern PixelReg,
    pattern UVReg,
    pattern WithVec,
    pixelPosToTex,
    pixelReg,
    uvReg,
    localToNdc,
    localPosToNdc,
    transform,
    WindowSize,
    mkWindowSize,
  )
where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Word (Word32)
import Foreign.Storable (Storable)
import qualified Geomancy as G
import qualified Geomancy.Elementwise as G
import qualified Geomancy.Transform as G
import Prelude hiding (lookup)

newtype NDCVec = NDCVec G.Vec2 deriving (Show, Num, Storable)

newtype PixelVec = PixelVec G.UVec2 deriving (Show, Num)

newtype UVVec = UVVec G.Vec2 deriving (Show, Storable, Num)

data WindowSize = WindowSize Word32 Word32 deriving (Show)

{-# COMPLETE WithVec #-}

-- pattern WithVec :: Vec u => Element u -> Element u -> u
pattern WithVec :: (Vec u) => Element u -> Element u -> u
pattern WithVec x y <- (unVec -> (x, y))

class Vec u where
  type Element u :: Type
  vec :: Element u -> Element u -> u
  unVec :: u -> (Element u, Element u)

-- TODO: a type class for all the various newtypes who wrap a G.XVec2
-- instance Vec G.UVec2 where
--   type Element G.UVec2 = G.Element G.UVec2
--   vec = G.uvec2
--   unVec (G.WithUVec2 x y) = (x, y)

instance Vec PixelVec where
  type Element PixelVec = G.Element G.UVec2
  vec = coerce G.uvec2
  unVec (PixelVec v) = let G.WithUVec2 x y = v in (x, y)

instance Vec NDCVec where
  type Element NDCVec = G.Element G.Vec2
  vec = coerce G.vec2
  unVec (NDCVec v) = let G.WithVec2 x y = v in (x, y)

instance Vec UVVec where
  type Element UVVec = G.Element G.Vec2
  vec = coerce G.vec2
  unVec (UVVec v) = let G.WithVec2 x y = v in (x, y)

instance Vec WindowSize where
  type Element WindowSize = Word32
  vec = WindowSize
  unVec (WindowSize w h) = (w, h)

mkWindowSize :: Word32 -> Word32 -> Maybe WindowSize
mkWindowSize w h
  | w >= 0 && h >= 0 = Just $ vec w h
  | otherwise = Nothing

newtype PixelRegion = PixelRegion G.UVec4 deriving (Show)

-- TODO: newtype to represent 1D pixel values ?
pixelReg :: Word32 -> Word32 -> Word32 -> Word32 -> PixelRegion
pixelReg x1 y1 x2 y2 = PixelRegion $ G.uvec4 x1 y1 x2 y2

{-# COMPLETE PixelReg #-}

pattern PixelReg :: Word32 -> Word32 -> Word32 -> Word32 -> PixelRegion -- TODO: make it bidirectional
pattern PixelReg x1 y1 x2 y2 <- PixelRegion (G.WithUVec4 x1 y1 x2 y2)

newtype UVRegion = UVRegion G.Vec4 deriving (Show)

uvReg :: Float -> Float -> Float -> Float -> UVRegion
uvReg u1 v1 u2 v2 = UVRegion $ G.vec4 u1 v1 u2 v2

{-# COMPLETE UVReg #-}

pattern UVReg :: Float -> Float -> Float -> Float -> UVRegion -- TODO: make it bidirectional
pattern UVReg u1 v1 u2 v2 <- UVRegion (G.WithVec4 u1 v1 u2 v2)

class Transform a where
  transform :: a -> G.Transform

instance Transform NDCVec where
  transform (WithVec x y) = G.translate x y 1

pixelPosToTex :: PixelVec -> PixelVec -> UVVec
pixelPosToTex (WithVec w h) (WithVec x y) =
  let nx = fromIntegral x / fromIntegral w
      ny = fromIntegral y / fromIntegral h
   in vec nx ny

pixelPosToNdc :: PixelVec -> WindowSize -> NDCVec
pixelPosToNdc (WithVec x y) (WithVec w h) =
  let nx = (fromIntegral x / fromIntegral w) * 2
      ny = (fromIntegral y / fromIntegral h) * 2
   in vec (nx - 1.0) (ny - 1.0)

pixelSizeToNdc :: PixelVec -> WindowSize -> NDCVec
pixelSizeToNdc (WithVec w h) (WithVec windowWidth windowHeight) =
  let nw = fromIntegral w / fromIntegral windowWidth
      nh = fromIntegral h / fromIntegral windowHeight
   in vec (nw * 2.0) (nh * 2.0)

localPosToNdc :: NDCVec -> UVVec -> NDCVec -> NDCVec
localPosToNdc (WithVec w h) (WithVec ox oy) (WithVec x y) =
  let x1 = localToNdc w ox x
      y1 = localToNdc h oy y
   in vec x1 y1

localToNdc :: Float -> Float -> Float -> Float
localToNdc ndcWidth factor x = x - ndcWidth * factor

class Normalized u where
  topLeft :: u
  topRight :: u
  bottomLeft :: u
  bottomRight :: u
  center :: u

instance Normalized NDCVec where
  topLeft = vec (-1.0) (-1.0)
  topRight = vec 1.0 (-1.0)
  bottomLeft = vec (-1.0) 1.0
  bottomRight = vec 1.0 1.0
  center = vec 0.0 0.0

instance Normalized UVVec where
  topLeft = vec 0.0 0.0
  topRight = vec 1.0 0.0
  bottomLeft = vec 0.0 1.0
  bottomRight = vec 1.0 1.0
  center = vec 0.5 0.5