{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
    embedIntoScreen,
    projection
  )
where

import Affine (Affine, srt, srt2affine)
import Camera (Camera, view)
import Control.Monad.Reader
import qualified Geomancy as G
import Measure
import Texture
import Vertex (Render (..), quad, vertex)

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

embedIntoScreen :: SpriteInScreen -> Affine
embedIntoScreen ss = a <> b
  where
    b = srt2affine pivot
    a = srt2affine $ srt (sx, sy) ss.rotation (x, y)
    pivot = let WithVec ox oy = ss.sprite.origin in srt (1, 1) 0 (-ox, -oy)
    G.WithVec2 sx sy = ss.scale
    WithVec x y = ss.position

instance (MonadReader ViewportSize m) => Render m SpriteInScreen where
  render ss = do
    ws <- ask
    let WithVec w h = ss.sprite.resolution
        UVReg2 auv buv cuv duv = ss.sprite.region
        a = vert ws 0 0 auv -- top left
        b = vert ws w 0 buv -- top right
        c = vert ws w h cuv -- bottom right
        d = vert ws 0 h duv -- bottom left
    return $ quad a b c d
    where
      vert ws x y uv = let xy = tr2 @PixelVec (projection ws <> embedIntoScreen ss) x y in vertex xy uv ss.sprite.texture
      projection (WithVec w h) = srt2affine $ srt (2 / fromIntegral w, 2 / fromIntegral h) 0 (-1, -1)

instance (MonadReader (Camera, PPU, ViewportSize) m) => Render m SpriteInWorld where
  render
    ss = do
      (cam, ppu, vps) <- ask
      let WithVec w h = ss.sprite.resolution
          UVReg2 auv buv cuv duv = ss.sprite.region
          a = vert cam ppu vps 0 0 auv -- top left
          b = vert cam ppu vps w 0 buv -- top right
          c = vert cam ppu vps w h cuv -- bottom right
          d = vert cam ppu vps 0 h duv -- bottom left
      return $ quad a b c d
      where
        vert cam ppu vps x y uv = let xy = tr2 @WorldVec (model cam ppu vps) x y in vertex xy uv ss.sprite.texture
        pivot = let WithVec ox oy = ss.sprite.origin in srt (1, 1) 0 (-ox, -oy)
        local ppu =
          let s = ppu_1 ppu
              G.WithVec2 sx sy = ss.scale
              WithVec x y = ss.position
           in srt (s * sx, -(s * sy)) ss.rotation (x, y) -- Place in world
        model cam ppu vps = projection vps ppu <> view cam <> srt2affine (local ppu) <> srt2affine pivot
        ppu_1 (PPU ppu) = 1 / ppu

projection :: ViewportSize -> PPU -> Affine
projection (WithVec w h) (PPU ppu) = srt2affine $ srt (s w, -(s h)) 0 (0, 0)
  where
    s x = (2 * ppu) / fromIntegral x