{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Txt
  ( Txt (origin), -- TODO more disciplined export
    text
  )
where

import Affine (srt, srt2affine)
import Atlas (Atlas, sprite)
import Data.Char (ord)
import Data.List (mapAccumL)
import Measure (PixelVec, Vec (vec, withFst, withVec))
import Render (Render (render))
import Sprite (Sprite (resolution), renderColored)
import Text.Printf (printf)
import Vertex (Color)

data Txt = Txt {str :: String, color :: Color, origin :: PixelVec, font :: Atlas}

text :: String -> Color -> Atlas -> Txt
text str c font = Txt {str = str, color = c, origin = vec 0 0, font = font}

instance Render Txt where
  render Txt {str, color, origin, font} tr =
    let (_, vs) = mapAccumL f 0 (write font str) in mconcat vs
    where
      f x gly = (withFst gly.resolution (+ x), renderColored gly color (tr <> base x <> pivot))
      pivot = withVec origin (\ox oy -> srt2affine $ srt (1, 1) 0 (-ox, -oy))
      base x = srt2affine $ srt (1, 1) 0 (x, 0)
      write font str = glyph font <$> str
      glyph font ch = sprite font $ printf "U+%04X" (ord ch)