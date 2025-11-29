{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module World
  ( PPU,
    ppu,
    projection,
    screen,
    world,
  )
where

import Affine (Affine, srt3)
import qualified Camera as Cam
import Measure (ViewportSize, pattern WithVec)

newtype PPU = PPU Float deriving (Num, Fractional)

ppu :: Float -> PPU
ppu = PPU

-- | world (world units) -> ndc
world :: ViewportSize -> PPU -> Cam.Camera -> Affine
world windowSize ppu camera = projection windowSize ppu <> Cam.view camera

-- | cam (world units) -> ndc
projection :: ViewportSize -> PPU -> Affine
projection (WithVec w h) (PPU ppu) = srt3 (sx, -sy) 0 (0, 0)
  where
    sx = ndc w
    sy = ndc h
    ndc x = (2 * ppu) / fromIntegral x

-- | screen (pixels) -> ndc
screen :: ViewportSize -> Affine
screen (WithVec w h) = srt3 (sx, sy) 0 (-1, -1)
  where
    sx = ndc w
    sy = ndc h
    ndc x = 2 / fromIntegral x
