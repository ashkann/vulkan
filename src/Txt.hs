{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

module Txt
  ( Txt (origin), -- TODO more disciplined export
    text,
    Font (..),
  )
where

import Affine (Affine, srt, srt2affine)
import Atlas (Atlas, sprite)
import Data.Char (ord)
import Data.List (mapAccumL)
import Measure (PixelVec, Vec (vec, withFst, withVec), ViewportSize, pattern WithVec)
import Render (Render (render))
import Sprite (In (..), Sprite (resolution), screen)
import Text.Printf (printf)
import Vertex (Color)

data Txt = Txt {str :: String, color :: Color, origin :: PixelVec}

text :: String -> Color -> Txt
text str c = Txt {str = str, color = c, origin = vec 0 0}

newtype Font = Font Atlas

instance Render (ViewportSize, Font) (In Txt PixelVec) where
  render (vps, font) In {object = (Txt {str, color, origin}), position = WithVec x0 y0, scale, rotation} =
    let (_, vs) = mapAccumL f x0 (write font str) in mconcat vs
    where
      f x gly = (withFst gly.resolution (+ x), render (scr <> base x <> pivot, Just color) gly)
      pivot = withVec origin (\ox oy -> srt2affine $ srt (1, 1) 0 (-ox, -oy))
      base x = srt2affine $ srt (1, 1) 0 (x, 0)
      scr = screen vps (scale, rotation, vec x0 y0) (vec 0 0)

instance Render (Affine, Font) Txt where
  render (tr, font) Txt {str, color, origin} =
    let (_, vs) = mapAccumL f 0 (write font str) in mconcat vs
    where
      f x gly = (withFst gly.resolution (+ x), render (tr <> base x <> pivot, Just color) gly)
      pivot = withVec origin (\ox oy -> srt2affine $ srt (1, 1) 0 (-ox, -oy))
      base x = srt2affine $ srt (1, 1) 0 (x, 0)

write :: Font -> String -> [Sprite]
write (Font font) str = glyph font <$> str
  where
    glyph font ch = sprite font $ printf "U+%04X" (ord ch)