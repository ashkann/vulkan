{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Txt
  ( Txt,
    text,
  )
where

import Atlas (Atlas, sprite)
import Control.Monad.State (MonadState (state), runState)
import Data.Char (ord)
import Measure (PixelVec, Vec (vec), ViewportSize, pattern WithVec)
import Sprite (In (..), putIn)
import Text.Printf (printf)
import Vertex (Color, Render (render, renderColored))

data Txt = Txt {str :: String, color :: Color}

text :: String -> Color -> Txt
text str c = Txt {str = str, color = c}

instance Render (ViewportSize, Atlas) (In Txt PixelVec) where
  render (vps, font) In {object = (Txt {str, color}), position = WithVec x0 y0} =
    let (text, _) = write font x0 y0 str
     in mconcat $ renderColored vps color <$> text
    where
      write font x0 y0 str = runState (traverse (\ch -> state (\x -> let g = glyph font ch; out = putIn g (vec @PixelVec x y0) in (out, x + 8))) str) x0
      glyph font ch = sprite font $ printf "U+%04X" (ord ch)