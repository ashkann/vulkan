{-# LANGUAGE DuplicateRecordFields #-}

module Sprite
  ( Sprite (..),
    SpriteInWorld (..),
    SpriteInScreen (..),
    putInWorld,
    putInScreen,
    rotateSprite,
    embedIntoScreen,
    bottomLeft,
  )
where

import qualified Geomancy as G
import Measure
import SRT (SRT, srt)
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
    position :: NDCVec,
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
  NDCVec ->
  SpriteInScreen
putInScreen sprite pos = SpriteInScreen {sprite = sprite, position = pos, rotation = 0, scale = G.vec2 1 1}

embedIntoScreen :: ViewportSize -> PixelVec -> SRT
embedIntoScreen (WithVec w h) (WithVec ox oy) = srt (2 / fromIntegral w, 2 / fromIntegral h) 0 (0, 0) <> srt (1, 1) 0 (-ox, -oy)