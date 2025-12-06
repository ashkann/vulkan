{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module World
  ( PPU,
    ppu,
    projection,
    screen,
    world,
    pixelToWorld,
  )
where

import Affine (Affine, srt3)
import qualified Camera as Cam
import Measure (ViewportSize (..))

newtype PPU = PPU Float deriving (Num, Fractional)

ppu :: Float -> PPU
ppu = PPU

pixelToWorld :: PPU -> Affine
pixelToWorld (PPU ppu) = srt3 (s, -s) 0 (0, 0)
  where
    s = 1 / ppu

-- | world (world units) -> ndc
world :: ViewportSize -> PPU -> Cam.Camera -> Affine
world windowSize ppu camera = projection windowSize ppu <> Cam.view camera

-- | cam (world units) -> ndc
projection :: ViewportSize -> PPU -> Affine
projection (ViewportSize w h) (PPU ppu) = srt3 (sx, -sy) 0 (0, 0)
  where
    sx = ndc w
    sy = ndc h
    ndc x = (2 * ppu) / fromIntegral x

-- | screen (pixels) -> ndc
screen :: ViewportSize -> Affine
screen (ViewportSize w h) = srt3 (sx, sy) 0 (-1, -1)
  where
    sx = ndc w
    sy = ndc h
    ndc x = 2 / fromIntegral x
