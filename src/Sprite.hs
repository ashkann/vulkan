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
    SpriteInWorld (..),
    SpriteInScreen (..),
    putInWorld,
    putInScreen,
    rotateSprite,
    bottomLeft,
    projection,
  )
where

import Affine (Affine, srt, srt2affine)
import Camera (Camera, view)
import Control.Monad.Reader
import qualified Geomancy as G
import Measure
import Texture
import Vertex (Quad, Render (..), Vertex, quad, vertex)

data Sprite = Sprite
  { texture :: DescriptorIndex,
    region :: UVRegion,
    resolution :: PixelVec,
    origin :: PixelVec
  }

data SpriteInWorld = SpriteInWorld
  { sprite :: Sprite,
    position :: WorldVec,
    rotation :: Float,
    scale :: G.Vec2
  }

data SpriteInScreen = SpriteInScreen
  { sprite :: Sprite,
    position :: PixelVec,
    rotation :: Float,
    scale :: G.Vec2
  }

-- | Put the sprite in the world with a given size.
putInWorld ::
  Sprite ->
  WorldVec ->
  SpriteInWorld
putInWorld sprite pos = SpriteInWorld {sprite = sprite, position = pos, rotation = 0, scale = G.vec2 1 1}

rotateSprite :: SpriteInWorld -> Float -> SpriteInWorld
rotateSprite s r = s {rotation = r}

putInScreen ::
  Sprite ->
  PixelVec ->
  SpriteInScreen
putInScreen sprite pos = SpriteInScreen {sprite = sprite, position = pos, rotation = 0, scale = G.vec2 1 1}

instance (MonadReader ViewportSize m) => Render m SpriteInScreen where
  render obj = asks (toQuad obj.sprite . vert)
    where
      vert t = ash @PixelVec (projection t <> model) obj.sprite.texture
      projection (WithVec w h) = srt2affine $ srt (2 / fromIntegral w, 2 / fromIntegral h) 0 (-1, -1)
      model =
        let G.WithVec2 sx sy = obj.scale
            WithVec x y = obj.position
            WithVec ox oy = obj.sprite.origin
            pivot = srt2affine $ srt (1, 1) 0 (-ox, -oy)
            local = srt2affine $ srt (sx, sy) obj.rotation (x, y)
         in local <> pivot

instance (MonadReader (Camera, PPU, ViewportSize) m) => Render m SpriteInWorld where
  render obj = asks (toQuad obj.sprite . vert)
    where
      vert t = ash @WorldVec (model t) obj.sprite.texture
      model (cam, ppu@(PPU _ppu), vps) =
        let s = 1 / _ppu
            G.WithVec2 sx sy = obj.scale
            WithVec x y = obj.position
            local = srt2affine $ srt (s * sx, -(s * sy)) obj.rotation (x, y) -- Place in world
            WithVec ox oy = obj.sprite.origin
            pivot = srt2affine $ srt (1, 1) 0 (-ox, -oy)
         in projection vps ppu <> view cam <> local <> pivot

ash :: forall u. (Tr u NDCVec) => Affine -> DescriptorIndex -> Element u -> Element u -> UVVec -> Vertex
ash tr tex x y uv = vertex (tr2 @u tr x y) uv tex

toQuad :: Sprite -> (Float -> Float -> UVVec -> Vertex) -> Quad
toQuad s vert = quad (vert 0 0 a) (vert w 0 b) (vert w h c) (vert 0 h d)
  where
    WithVec w h = s.resolution
    UVReg2 a b c d = s.region

projection :: ViewportSize -> PPU -> Affine
projection (WithVec w h) (PPU ppu) = srt2affine $ srt (s w, -(s h)) 0 (0, 0)
  where
    s x = (2 * ppu) / fromIntegral x