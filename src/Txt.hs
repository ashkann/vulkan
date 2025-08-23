{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Txt
  ( Txt,
    text,
    Font (..),
  )
where

import Atlas (Atlas, sprite)
import Camera (Camera)
import Data.Char (ord)
import Data.List (mapAccumL)
import Measure (PPU, PixelVec, Vec (vec), ViewportSize, WorldVec, pattern WithVec)
import Sprite (In (..), Sprite, screenAffine, worldAffine)
import Text.Printf (printf)
import Vertex (Color, Render (render))

data Txt = Txt {str :: String, color :: Color}

text :: String -> Color -> Txt
text str c = Txt {str = str, color = c}

newtype Font = Font Atlas

instance Render (ViewportSize, Font) (In Txt PixelVec) where
  render (vps, font) In {object = (Txt {str, color}), position = WithVec x0 y0, scale, rotation} =
    let (_, vs) = mapAccumL f x0 (write font str) in mconcat vs
    where
      f x gly = (x + 8.0, render (tr x, Just color) gly)
      tr x = screenAffine vps (scale, rotation, vec x y0) (vec 0 0)

instance Render (Camera, PPU, ViewportSize, Font) (In Txt WorldVec) where
  render (cam, ppu, vps, font) In {object = (Txt {str, color}), position = WithVec x0 y0, scale, rotation} =
    let (_, vs) = mapAccumL f x0 (write font str) in mconcat vs
    where
      f x gly = (x + 0.1, render (tr x, Just color) gly)
      tr x = worldAffine (cam, ppu, vps) (scale, rotation, vec x y0) (vec 0 0)

write :: Font -> String -> [Sprite]
write (Font font) str = glyph font <$> str
  where
    glyph font ch = sprite font $ printf "U+%04X" (ord ch)