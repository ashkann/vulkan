{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Camera
  ( Camera(..),
    rotateCamera,
    moveCamera,
    zoomCameraTo,
    zoomInCamera,
    view,
    defaultCamera,
  )
where

import Affine
import Measure

data Camera = Camera {position :: WorldVec, rotation :: Float, zoom :: Float}

defaultCamera :: Camera
defaultCamera = Camera {position = vec 0 0, rotation = 0, zoom = 1}

rotateCamera :: Float -> Camera -> Camera
rotateCamera r cam = cam {rotation = cam.rotation + r}

moveCamera :: WorldVec -> Camera -> Camera
moveCamera (WithVec dx dy) cam = cam {position = cam.position + vec dxCam dyCam}
  where
    (dxCam, dyCam) = Affine.apply (srt2affine $ srt (1, 1) cam.rotation (0, 0)) (dx, dy)

zoomCameraTo :: Float -> Camera -> Camera
zoomCameraTo z cam = cam {zoom = z}

zoomInCamera :: Float -> Camera -> Camera
zoomInCamera s cam = zoomCameraTo (cam.zoom + s) cam

view :: Camera -> Affine
view Camera {position = WithVec x y, rotation, zoom = z} = rotateAndZoom <> lookAt
  where
    lookAt = srt2affine $ srt (1, 1) 0 (-x, -y)
    rotateAndZoom = srt2affine $ srt (z, z) (-rotation) (0, 0)
