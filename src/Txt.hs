{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Txt
  ( Txt,
    text,
  )
where

import Atlas (Atlas, sprite)
import Control.Monad.State (MonadState (state), runState)
import Data.Char (ord)
import Measure (PixelVec, Vec (vec), ViewportSize, pattern WithVec)
import Sprite (putInScreen)
import Text.Printf (printf)
import Vertex (Color, Render (render, renderColored))

data Txt = Txt {str :: String, color :: Color, pos :: PixelVec}

text :: String -> Color -> PixelVec -> Txt
text str c pos = Txt {str = str, color = c, pos = pos}

instance Render (ViewportSize, Atlas) Txt where
  render (vps, font) Txt {str, color, pos = WithVec x0 y0} =
    let (text, _) = write font x0 y0 str
     in mconcat $ renderColored vps color <$> text
    where
      write font x0 y0 str = runState (traverse (\ch -> state (\x -> let g = glyph font ch; out = putInScreen g (vec x y0) in (out, x + 8))) str) x0
      glyph font ch = sprite font $ printf "U+%04X" (ord ch)