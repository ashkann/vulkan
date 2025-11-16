{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Render
  ( Render (..),
    projection,
    world,
    screen,
    applyObject,
  )
where

import Affine (Affine, scale, scaleXY, srt3)
import Camera (Camera, view)
import Measure hiding (transform)
import Vertex (Vertex, applyVert)

class Render obj where
  render :: obj -> [Vertex PixelVec]
  {-# MINIMAL render #-}

instance (Render a) => Render [a] where
  render :: [a] -> [Vertex PixelVec]
  render = (render =<<)

applyObject :: (Render obj, Vec v, Element v ~ Float) => Affine -> obj -> [Vertex v] -- TODO simplify types
applyObject m obj = applyVert m <$> render obj

scalePixelToWorld :: PPU -> Affine
scalePixelToWorld = withRecipPPU (\ppu_1 -> Affine.scale $ scaleXY ppu_1 (-ppu_1))

-- instance Render Node where
--   render
--     Node
--       { content,
--         position,
--         scale,
--         rotation,
--         origin
--       } =
--       render content

-- instance Render [Node] where
--   render ns = concatMap render ns

-- render object (tr <> translate position <> scalePixelToWorld (ppu 100) <> sr scale rotation <> Affine.origin origin)
-- render content (tr <> translate position <> scalePixelToWorld (ppu 100) <> sr scale rotation <> Affine.origin origin)

-- instance Render Node where
--   render
--     Node
--       { content,
--         position,
--         scale,
--         rotation,
--         origin
--       }
--     tr =
--       render content (tr <> srt scale rotation position <> Affine.origin origin)

world :: Camera -> PPU -> ViewportSize -> Affine
world cam ppu vps = projection vps ppu <> view cam

screen :: ViewportSize -> Affine
screen (WithVec w h) = srt3 (s w, s h) 0 (-1, -1)
  where
    s x = 2 / fromIntegral x

projection :: ViewportSize -> PPU -> Affine
projection (WithVec w h) ppu = srt3 (s w, -(s h)) 0 (0, 0)
  where
    s x = withPPU (\ppu -> (2 * ppu) / fromIntegral x) ppu