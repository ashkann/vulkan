{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Render
  ( Render (..),
    In (..),
    putIn,
    setRotation,
    setScale,
    projection,
    world,
    screen,
    setOrigin,
    srtPutIn,
  )
where

import Affine (Affine, Rotation, Scale, noRatation, noScale, scaleXY, sr, srt, srt3, translate)
import qualified Affine
import Camera (Camera, view)
import qualified Data.Vector.Storable as SV
import Measure
import Vertex (Vertex)

class Render obj where
  render :: obj -> Affine -> SV.Vector Vertex -- TODO any Traversable would do ?

scalePixelToWorld :: PPU -> Affine
scalePixelToWorld = withRecipPPU (\ppu_1 -> Affine.scale $ scaleXY ppu_1 (-ppu_1))

data In obj vec = In {object :: obj, position :: vec, rotation :: Rotation, scale :: Scale, origin :: PixelVec}

putIn :: obj -> vec -> In obj vec
putIn obj pos = In {object = obj, position = pos, rotation = noRatation, scale = noScale, origin = vec 0 0}

srtPutIn :: obj -> Scale -> Rotation -> vec -> PixelVec -> In obj vec
srtPutIn obj s r pos o = In {object = obj, position = pos, rotation = r, scale = s, origin = o}

setRotation :: Rotation -> In obj vec -> In obj vec
setRotation r s = s {rotation = r}

setScale :: Scale -> In obj vec -> In obj vec
setScale k s = s {scale = k}

setOrigin :: PixelVec -> In obj vec -> In obj vec
setOrigin o s = s {origin = o}

instance (Render obj) => Render (In obj WorldVec) where
  render
    In
      { object,
        position,
        scale,
        rotation,
        origin
      }
    tr =
      render object (tr <> translate position <> scalePixelToWorld (ppu 100) <> sr scale rotation <> Affine.origin origin)

instance (Render obj) => Render (In obj PixelVec) where
  render
    In
      { object,
        position,
        scale,
        rotation,
        origin
      }
    tr =
      render object (tr <> srt scale rotation position <> Affine.origin origin)

world :: Camera -> PPU -> ViewportSize -> Affine
world cam ppu vps = projection vps ppu <> view cam

screen :: ViewportSize -> Affine
screen (WithVec w h) = srt3 (s w, s h) 0 (-1, -1)
  where
    s x = 2 / fromIntegral x

projection :: ViewportSize -> PPU -> Affine
projection (WithVec w h) ppu = srt3 (s w, -(s h)) 0 (0, 0)
  where
    s x = withPPU (\ppu -> (2 * ppu) / fromIntegral x) ppu