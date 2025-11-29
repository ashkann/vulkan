{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module World
  ( pixelSizeToWorld,
    PPU (),
    withPPU,
    ppu,
    world,
    projection,
    screen,
  )
where

import Affine (Affine, srt3)
import Camera
import Measure (PixelVec, Vec (vec), ViewportSize, WorldVec, pattern WithVec)

newtype PPU = PPU Float deriving (Num, Fractional)

ppu :: Float -> PPU
ppu = PPU

withPPU :: (Float -> t) -> PPU -> t
withPPU f ppu = let PPU x = ppu in f x

pixelSizeToWorld :: PPU -> PixelVec -> WorldVec
pixelSizeToWorld (PPU ppu) (WithVec w h) =
  let x = w / ppu
      y = h / ppu
   in vec x y

world :: Camera -> PPU -> ViewportSize -> Affine
world cam ppu vps = projection vps ppu <> view cam

projection :: ViewportSize -> PPU -> Affine
projection (WithVec w h) ppu = srt3 (world2Ndc w ppu, -(world2Ndc h ppu)) 0 (0, 0)
  where
    world2Ndc x = withPPU (\ppu -> (2 * ppu) / fromIntegral x)

screen :: ViewportSize -> Affine
screen (WithVec w h) = srt3 scale 0 center
  where
    center = (-1, -1)
    scale = (pixelToNdc w, pixelToNdc h)
    pixelToNdc x = 2 / fromIntegral x
