{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Measure
  ( PixelVec,
    NDCVec,
    UVVec,
    LocalVec,
    WorldVec,
    Vec (..),
    Tr (..),
    PixelRegion (..),
    UVRegion,
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
    ViewportSize,
    mkWindowSize,
    pattern UVReg2,
    pixelSizeToWorld,
    PPU (..),
  )
where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Word (Word32)
import Foreign.Storable (Storable)
import qualified Geomancy as G
import qualified Geomancy.Elementwise as G
import qualified Geomancy.Transform as G
import SRT (SRT)
import qualified SRT
import Prelude hiding (lookup)

newtype NDCVec = NDCVec G.Vec2 deriving (Show, Num, Storable)

newtype PixelVec = PixelVec G.Vec2 deriving (Show, Num)

newtype UVVec = UVVec G.Vec2 deriving (Show, Storable, Num)

newtype LocalVec = LocalVec G.Vec2

data ViewportSize = ViewportSize Word32 Word32 deriving (Show)

newtype WorldVec = WorldVec G.Vec2 deriving (Show, Num)

{-# COMPLETE WithVec #-}

pattern WithVec :: (Vec v) => Element v -> Element v -> v
pattern WithVec x y <- (unvec -> (x, y))

class Vec v where
  type Element v :: Type
  vec :: Element v -> Element v -> v
  fromTuple :: (Element v, Element v) -> v
  fromTuple (x, y) = vec x y
  unvec :: v -> (Element v, Element v)

class (Vec u, Vec v, Element v ~ Float, Element u ~ Float) => Tr u v where
  tr :: SRT -> u -> v
  tr m (WithVec x y) = tr2 @u m x y
  tr2 :: SRT -> Element u -> Element u -> v
  tr2 m x y = let (x', y') = SRT.apply m x y in vec x' y'

instance Vec PixelVec where
  type Element PixelVec = G.Element G.Vec2
  vec = coerce G.vec2
  unvec (PixelVec v) = let G.WithVec2 x y = v in (x, y)

instance Vec NDCVec where
  type Element NDCVec = G.Element G.Vec2
  vec = coerce G.vec2
  unvec (NDCVec v) = let G.WithVec2 x y = v in (x, y)

instance Vec LocalVec where
  type Element LocalVec = G.Element G.Vec2
  vec = coerce G.vec2
  unvec (LocalVec v) = let G.WithVec2 x y = v in (x, y)

instance Vec UVVec where
  type Element UVVec = G.Element G.Vec2
  vec = coerce G.vec2
  unvec (UVVec v) = let G.WithVec2 x y = v in (x, y)

instance Vec ViewportSize where
  type Element ViewportSize = Word32
  vec = ViewportSize
  unvec (ViewportSize w h) = (w, h)

instance Vec WorldVec where
  type Element WorldVec = G.Element G.Vec2
  vec = coerce G.vec2
  unvec (WorldVec v) = let G.WithVec2 x y = v in (x, y)

instance Tr NDCVec NDCVec

instance Tr WorldVec NDCVec

instance Tr WorldVec WorldVec

instance Tr PixelVec WorldVec

mkWindowSize :: Word32 -> Word32 -> Maybe ViewportSize
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

{-# COMPLETE UVReg2 #-}

pattern UVReg2 :: UVVec -> UVVec -> UVVec -> UVVec -> UVRegion
pattern UVReg2 a b c d <- (uvRegCorners -> (a, b, c, d))

uvRegCorners :: UVRegion -> (UVVec, UVVec, UVVec, UVVec)
uvRegCorners (UVReg u1 v1 u2 v2) =
  let a = vec u1 v1
      b = vec u2 v1
      c = vec u2 v2
      d = vec u1 v2
   in (a, b, c, d)

class Transform a where
  transform :: a -> G.Transform

instance Transform NDCVec where
  transform (WithVec x y) = G.translate x y 1

pixelPosToTex :: PixelVec -> PixelVec -> UVVec
pixelPosToTex (WithVec w h) (WithVec x y) =
  let u = x / w
      v = y / h
   in vec u v

pixelPosToNdc :: ViewportSize -> PixelVec -> NDCVec
pixelPosToNdc (WithVec w h) (WithVec x y) =
  let nx = (x / fromIntegral w) * 2
      ny = (y / fromIntegral h) * 2
   in vec (nx - 1.0) (ny - 1.0)

pixelSizeToNdc :: ViewportSize -> PixelVec -> NDCVec
pixelSizeToNdc (WithVec ww wh) (WithVec w h) =
  let nw = w / fromIntegral ww
      nh = h / fromIntegral wh
   in vec (nw * 2.0) (nh * 2.0)

localPosToNdc :: NDCVec -> LocalVec -> LocalVec -> NDCVec
localPosToNdc size origin position =
  let (WithVec w h) = size
      (WithVec ox oy) = origin
      (WithVec x y) = position
      lx = localToNdc w ox x
      ly = localToNdc h oy y
   in vec lx ly
  where
    localToNdc ndcWidth factor x = x - ndcWidth * factor

newtype PPU = PPU Float deriving (Num)

pixelSizeToWorld :: PPU -> PixelVec -> WorldVec
pixelSizeToWorld (PPU ppu) (WithVec w h) =
  let x = w / ppu
      y = h / ppu
   in vec x y

class (Vec v, Element v ~ Float) => Normalized v where
  top :: Element v
  left :: Element v
  bottom :: Element v
  right :: Element v
  topLeft :: v
  topLeft = vec (left @v) (top @v)
  topRight :: v
  topRight = vec (right @v) (top @v)
  bottomLeft :: v
  bottomLeft = vec (left @v) (bottom @v)
  bottomRight :: v
  bottomRight = vec (right @v) (bottom @v)
  center :: v
  center = vec (x / 2) (y / 2) where x = right @v + left @v; y = top @v + bottom @v

instance Normalized NDCVec where
  top = -1.0
  left = -1.0
  bottom = 1.0
  right = 1.0

instance Normalized LocalVec where
  top = 0
  left = 0
  right = 1
  bottom = 1

instance Normalized UVVec where
  top = 0
  left = 0
  right = 1
  bottom = 1
