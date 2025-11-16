{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Txt
  ( Txt (origin), -- TODO more disciplined export
    text,
  )
where

import Affine (srt3, translateX)
import Atlas (Atlas, sprite)
import Control.Monad (join)
import Data.Char (ord)
import Data.List (mapAccumL)
import Measure (PixelVec, Vec (vec, withFst, withVec))
import Render (Render (..))
import Sprite (Sprite (resolution))
import Text.Printf (printf)
import Vertex (Color, applyVert, setColor)

data Txt = Txt {str :: String, color :: Color, origin :: PixelVec, font :: Atlas}

text :: String -> Color -> Atlas -> Txt
text str c font = Txt {str = str, color = c, origin = vec 0 0, font = font}

instance Render Txt where
  render Txt {str, color, origin, font} =
    let (_, vs) = mapAccumL f 0 (write font str) in join vs
    where
      f x gly = (withFst gly.resolution (+ x), g x <$> render gly)
      g x = setColor color . applyVert (translateX x <> pivot)
      pivot = withVec origin (\ox oy -> srt3 (1, 1) 0 (-ox, -oy))
      write font str = glyph font <$> str
      glyph font ch = sprite font $ printf "U+%04X" (ord ch)