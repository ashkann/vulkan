{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Txt
  ( Txt,
    text,
    Font(..),
  )
where

import Atlas (Atlas, sprite)
import Camera (Camera)
import Control.Monad.State (MonadState (state), runState)
import Data.Char (ord)
import Measure (PPU, PixelVec, Vec (vec), ViewportSize, WorldVec, pattern WithVec)
import Sprite (In (..), putIn)
import Text.Printf (printf)
import Vertex (Color, Render (render, renderColored))

data Txt = Txt {str :: String, color :: Color}

text :: String -> Color -> Txt
text str c = Txt {str = str, color = c}

newtype Font = Font Atlas

instance Render (ViewportSize, Font) (In Txt PixelVec) where
  render (vps, Font font) In {object = (Txt {str, color}), position = WithVec x0 y0} =
    let (text, _) = write font x0 y0 str
     in mconcat $ renderColored vps color <$> text
    where
      write font x0 y0 str = runState (traverse (\ch -> state (\x -> let g = glyph font ch; out = putIn g (vec @PixelVec x y0) in (out, x + 8))) str) x0
      glyph font ch = sprite font $ printf "U+%04X" (ord ch)

instance Render (Camera, PPU, ViewportSize, Font) (In Txt WorldVec) where
  render (cam, ppu, vps, Font font) In {object = (Txt {str, color}), position = WithVec x0 y0} =
    let (text, _) = write font x0 y0 str
     in mconcat $ renderColored (cam, ppu, vps) color <$> text
    where
      write font x0 y0 str = runState (traverse (\ch -> state (\x -> let g = glyph font ch; out = putIn g (vec @WorldVec x y0) in (out, x + 0.08))) str) x0
      glyph font ch = sprite font $ printf "U+%04X" (ord ch)