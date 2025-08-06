{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Sprite
  ( Sprite (..),
    SpriteInWorld (..),
    SpriteInScreen (..),
    putInWorld,
    putInScreen,
    rotateSprite,
    bottomLeft,
    embedIntoScreen,
  )
where

import Affine (Affine, srt, srt2affine)
import qualified Geomancy as G
import Measure
import Texture

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