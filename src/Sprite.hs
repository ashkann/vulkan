{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Sprite
  ( Sprite (..),
    In (..),
    putIn,
    rotateSprite,
    bottomLeft,
    projection,
  )
where

import Affine (Affine, srt, srt2affine)
import Camera (Camera, view)
import qualified Data.Vector.Storable as SV
import qualified Geomancy as G
import Measure
import Texture
import Vertex (Render (..), Vertex, vertex)

data Sprite = Sprite
  { texture :: DescriptorIndex,
    region :: UVRegion,
    resolution :: PixelVec,
    origin :: PixelVec
  }

data In obj vec = In {object :: obj, position :: vec, rotation :: Float, scale :: G.Vec2}

rotateSprite :: In WorldVec Sprite -> Float -> In WorldVec Sprite
rotateSprite s r = s {rotation = r}

putIn ::
  obj ->
  vec ->
  In obj vec
putIn obj pos = In {object = obj, position = pos, rotation = 0, scale = G.vec2 1 1}

instance Render ViewportSize (In Sprite PixelVec) where
  render vps obj = vertices obj.object (xy vps)
    where
      xy t = tr2 @PixelVec (projection t <> model)
      projection (WithVec w h) = srt2affine $ srt (2 / fromIntegral w, 2 / fromIntegral h) 0 (-1, -1)
      model =
        let G.WithVec2 sx sy = obj.scale
            WithVec x y = obj.position
            WithVec ox oy = obj.object.origin
            pivot = srt2affine $ srt (1, 1) 0 (-ox, -oy)
            local = srt2affine $ srt (sx, sy) obj.rotation (x, y)
         in local <> pivot

instance Render (Camera, PPU, ViewportSize) (In Sprite WorldVec) where
  render env obj = vertices obj.object (xy env)
    where
      xy t = tr2 @WorldVec (model t)
      model (cam, ppu@(PPU _ppu), vps) =
        let s = 1 / _ppu
            G.WithVec2 sx sy = obj.scale
            WithVec x y = obj.position
            local = srt2affine $ srt (s * sx, -(s * sy)) obj.rotation (x, y) -- Place in world
            WithVec ox oy = obj.object.origin
            pivot = srt2affine $ srt (1, 1) 0 (-ox, -oy)
         in projection vps ppu <> view cam <> local <> pivot

vertices :: Sprite -> (Element PixelVec -> Element PixelVec -> NDCVec) -> SV.Vector Vertex
vertices s xy = SV.fromList [va, vb, vc, vc, vd, va]
  where
    WithVec w h = s.resolution
    UVReg2 a b c d = s.region
    va = vertex (xy 0 0) a s.texture
    vb = vertex (xy w 0) b s.texture
    vc = vertex (xy w h) c s.texture
    vd = vertex (xy 0 h) d s.texture

projection :: ViewportSize -> PPU -> Affine
projection (WithVec w h) (PPU ppu) = srt2affine $ srt (s w, -(s h)) 0 (0, 0)
  where
    s x = (2 * ppu) / fromIntegral x