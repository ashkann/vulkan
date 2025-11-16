{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Camera
  ( Camera (..),
    rotate,
    move,
    cameraZoom,
    view,
    defaultCamera,
    moveUp,
    moveDown,
    moveLeft,
    moveRight,
    rotateCw,
    rotateCcw,
    zoomIn,
    zoomOut
  )
where

import Affine hiding (rotate)
import Measure

data Camera = Camera {position :: WorldVec, rotation :: Float, zoom :: Float}

defaultCamera :: Camera
defaultCamera = Camera {position = vec 0 0, rotation = 0, zoom = 1}

rotate :: Float -> Camera -> Camera
rotate r cam = cam {rotation = cam.rotation + r}

rotateCcw :: Float -> Camera -> Camera
rotateCcw = rotate

rotateCw :: Float -> Camera -> Camera
rotateCw dr = rotate (-dr)

move :: WorldVec -> Camera -> Camera
move (WithVec dx dy) cam = cam {position = cam.position + vec dxCam dyCam}
  where
    (dxCam, dyCam) = Affine.applyXY (srt3 (1, 1) cam.rotation (0, 0)) (dx, dy)

moveUp :: Float -> Camera -> Camera
moveUp d = move $ vec 0 d

moveDown :: Float -> Camera -> Camera
moveDown d = move $ vec 0 (-d)

moveLeft :: Float -> Camera -> Camera
moveLeft d = move $ vec (-d) 0

moveRight :: Float -> Camera -> Camera
moveRight d = move $ vec d 0

zoomTo :: Float -> Camera -> Camera
zoomTo z cam = cam {zoom = z}

cameraZoom :: Float -> Camera -> Camera
cameraZoom ds cam = zoomTo (cam.zoom + ds) cam

zoomIn :: Float -> Camera -> Camera
zoomIn = cameraZoom

zoomOut :: Float -> Camera -> Camera
zoomOut dz = cameraZoom (-dz)

view :: Camera -> Affine
view Camera {position = WithVec x y, rotation, zoom = z} = rotateAndZoom <> lookAt
  where
    lookAt = srt3 (1, 1) 0 (-x, -y)
    rotateAndZoom = srt3 (z, z) (-rotation) (0, 0)


-- worldTime :: (Monad io) => TimeSeconds -> World -> io World
-- worldTime (TimeSeconds dt) w = return $ w {camera = foldl (\cam act -> act cam) w.camera cameraActions}