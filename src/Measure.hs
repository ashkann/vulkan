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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Measure
  ( PixelVec,
    NDCVec,
    UVVec,
    LocalVec,
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
    localPosToNdc,
    transform,
    WindowSize,
    mkWindowSize,
    topLeft,
    topRight,
    bottomLeft,
    bottomRight,
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

newtype LocalVec = LocalVec G.Vec2

data WindowSize = WindowSize Word32 Word32 deriving (Show)

{-# COMPLETE WithVec #-}

-- pattern WithVec :: Vec u => Element u -> Element u -> u
pattern WithVec :: (Vec u) => Element u -> Element u -> u
pattern WithVec x y <- (unVec -> (x, y))

class Vec u where
  type Element u :: Type
  vec :: Element u -> Element u -> u
  unVec :: u -> (Element u, Element u)

instance Vec PixelVec where
  type Element PixelVec = G.Element G.UVec2
  vec = coerce G.uvec2
  unVec (PixelVec v) = let G.WithUVec2 x y = v in (x, y)

instance Vec NDCVec where
  type Element NDCVec = G.Element G.Vec2
  vec = coerce G.vec2
  unVec (NDCVec v) = let G.WithVec2 x y = v in (x, y)

instance Vec LocalVec where
  type Element LocalVec = G.Element G.Vec2
  vec = coerce G.vec2
  unVec (LocalVec v) = let G.WithVec2 x y = v in (x, y)

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
pixelSizeToNdc (WithVec w h) (WithVec ww wh) =
  let nw = fromIntegral w / fromIntegral ww
      nh = fromIntegral h / fromIntegral wh
   in vec (nw * 2.0) (nh * 2.0)

localPosToNdc :: NDCVec -> LocalVec -> UVVec -> NDCVec
localPosToNdc (WithVec w h) (WithVec ox oy) (WithVec x y) =
  let lx = localToNdc w ox x
      ly = localToNdc h oy y
   in vec lx ly
  where
    localToNdc ndcWidth factor x = x - ndcWidth * factor

class (Vec u) => Normalized u where
  top :: Element u
  left :: Element u
  bottom :: Element u
  right :: Element u

-- center :: u

topLeft :: forall u. (Normalized u) => u
topLeft = vec (left @u) (top @u)

topRight :: forall u. (Normalized u) => u
topRight = vec (left @u) (top @u)

bottomLeft :: forall u. (Normalized u) => u
bottomLeft = vec (left @u) (bottom @u)

bottomRight :: forall u. (Normalized u) => u
bottomRight = vec (right @u) (bottom @u)

instance Normalized NDCVec where
  top = -1.0
  left = -1.0
  bottom = 1.0
  right = 1.0

-- center = vec 0.0 0.0

instance Normalized LocalVec where
  top = 0
  left = 0
  right = 1
  bottom = 1

-- center = vec 0.5 0.5