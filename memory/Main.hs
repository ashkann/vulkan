{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Affine (Affine, inverse, origin, srt, translate)
import Atlas (Atlas)
import qualified Atlas
import qualified Camera as Cam
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Managed (Managed, MonadIO (liftIO), managed, runManaged)
import Data.Foldable (foldlM)
import Data.Functor (($>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as SV
import Foreign (Ptr, Word32)
import Foreign.Storable (Storable (..), sizeOf)
import qualified Init
import Measure (PPU, PixelVec, Tr (tr), Vec (vec), ViewportSize, WorldVec, pattern WithVec)
import qualified Measure
import Render (In (object), noScale, projection, putIn, rotateDegree, scaleXY, setRotation, setScale, srtPutIn)
import qualified Render
import qualified SDL
import Sprite
import qualified System.Random as Random
import qualified Texture as Tex
import Txt (text)
import qualified Txt (Txt (origin))
import Utils
import qualified Vertex as Vert
import qualified Vulkan as Vk
import qualified Vulkan as VkCommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan as VkDevice (Device (..))
import qualified Vulkan as VkExtent2D (Extent2D (..))
import qualified Vulkan as VkFenceCreateInfo (FenceCreateInfo (..))
import qualified Vulkan as VkPipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan as VkPresentInfoKHR (PresentInfoKHR (..))
import qualified Vulkan as VkRect2D (Rect2D (..))
import qualified Vulkan as VkRenderingAttachmentInfo (RenderingAttachmentInfo (..))
import qualified Vulkan as VkRenderingInfo (RenderingInfo (..))
import qualified Vulkan as VkSubmitInfo (SubmitInfo (..))
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified VulkanMemoryAllocator as Vma
import Prelude hiding (init)

newtype TimeSeconds = TimeSeconds Float

frameData :: Game -> World -> FrameData
frameData g w = FrameData {verts = mconcat $ render <$> scene w}
  where
    render (Object obj W) = Render.render obj (world g)
    render (Object obj S) = Render.render obj (screen g)

data Frame = Frame
  { pool :: Vk.CommandPool,
    renderCmd :: Vk.CommandBuffer,
    copyCmd :: Vk.CommandBuffer,
    fence :: Vk.Fence,
    imageAvailable :: Vk.Semaphore,
    renderFinished :: Vk.Semaphore,
    copyFinished :: Vk.Semaphore,
    staging :: (Vk.Buffer, Ptr ()),
    vertex :: Vk.Buffer,
    targetImage :: Vk.Image,
    targetView :: Vk.ImageView
  }

-- TODO: run inside a MonadError instance
main :: IO ()
main = runManaged $ do
  (window, vulkan, surface) <- withSDL2VulkanWindow windowSize
  (gpu, gfx, present, portability, gpuName) <- Init.pickGPU vulkan surface
  say "Vulkan" $ "GPU: " ++ gpuName ++ ", present: " ++ show present.index ++ ", graphics: " ++ show gfx.index
  device <- Init.withDevice gpu gfx present portability <* say "Vulkan" "Created device"
  commandPool <-
    let info =
          Vk.zero
            { VkCommandPoolCreateInfo.queueFamilyIndex = gfx.index,
              VkCommandPoolCreateInfo.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
            }
     in managed $ Vk.withCommandPool device info Nothing bracket
  allocator <- Init.withMemoryAllocator vulkan gpu device <* say "Vulkan" "Created VMA allocator"
  let descriptorCount = 16
  descSetLayout <- Init.descriptorSetLayout device descriptorCount
  descPool <- Init.descriptorPool device 1000
  descSet <- Init.descriptorSet device descSetLayout descPool
  sampler <- Utils.repeatingSampler device
  gfxQueue <- Vk.getDeviceQueue device gfx.index 0 <* say "Vulkan" "Got graphics queue"

  [atlas, font] <- Atlas.withAtlas allocator device commandPool gfxQueue descSet sampler ["out/memory/atlas.atlas", "out/font.atlas"]
  say "Engine" "Atlases loaded"

  SDL.showWindow window <* say "SDL" "Show window"
  SDL.raiseWindow window <* say "SDL" "Raise window"
  (SDL.cursorVisible SDL.$= False) <* say "SDL" "Dsiable cursor"

  swapchain@(_, swapchainExtent, swapchainImages) <-
    Init.withSwapChain
      gpu
      device
      surface
      gfx
      present
      windowSize
  let frameCount = V.length swapchainImages
  say "Vulkan" "Created swapchain"

  say "Engine" $ "Frame count is " ++ show frameCount

  pipelineLayout <-
    let info = Vk.zero {VkPipelineLayoutCreateInfo.setLayouts = [descSetLayout]}
     in managed $ Vk.withPipelineLayout device info Nothing bracket
  pipeline <- Init.createPipeline device swapchainExtent pipelineLayout Vert.grpahicsPipelineVertexInputState
  presentQueue <- Vk.getDeviceQueue device present.index 0 <* say "Vulkan" "Got present queue"

  let stagingBufferSize = 1048576
      maxVertCount = 10000
      vertexBufferSize = fromIntegral $ sizeOf (undefined :: Vert.Vertex) * maxVertCount
  frames <- withFrames device gfx.index allocator stagingBufferSize vertexBufferSize frameCount
  w0 <- liftIO $ world0 atlas font
  let shutdown = say "Engine" "Shutting down ..." *> Vk.deviceWaitIdle device
      game w = Game {windowSize = windowSize, font = Font font, camera = w.camera, ppu = ppu, atlas = atlas} -- TODO move 'World' into 'Game'
   in say "Engine" "Entering the main loop"
        *> mainLoop
          shutdown
          (\w -> frameData (game w) w)
          (\n d -> let f = frames ! (n `mod` frameCount) in frame device gfxQueue presentQueue pipeline pipelineLayout swapchain descSet f d)
          worldTime
          worldEvent
          w0

-- data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Eq, Ord, Enum)

-- newtype Score = Score (Digit, Digit, Digit)

-- mkScore :: Int -> Maybe Score
-- mkScore n
--   | n >= 0 && n <= 999 = Just $ Score (toEnum $ n `div` 100, toEnum $ (n `div` 10) `mod` 10, toEnum $ n `mod` 10)
--   | otherwise = Nothing

-- ashkan :: Atlas -> Int -> [Object]
-- ashkan atlas n =
--   let digits = Map.fromList [(d, Atlas.spriteIndexed atlas "digit" (fromIntegral . fromEnum $ d) Measure.texCenter windowSize) | d <- [D0 .. D9]]
--       score (Score (a, b, c)) = [x, y, z]
--         where
--           x =
--             Object
--               { sprite = digits Map.! a,
--                 transform = spriteTranslation Measure.ndcCenter,
--                 tr = G.translate 0 0 0
--               }
--           w1 = let Measure.NormalizedDeviceWH w _ = (digits Map.! a).size in Measure.ndcSize w 0
--           y1 = Measure.translate w1 Measure.ndcCenter
--           y =
--             Object
--               { sprite = digits Map.! b,
--                 transform = spriteTranslation y1,
--                 tr = G.translate 0 0 0
--               }
--           w2 = let Measure.NormalizedDeviceWH w _ = (digits Map.! b).size in Measure.ndcSize w 0
--           y2 = Measure.translate w2 y1
--           z =
--             Object
--               { sprite = digits Map.! c,
--                 transform = spriteTranslation y2,
--                 tr = G.translate 0 0 0
--               }
--    in maybe [] score $ mkScore n

newtype Row = Row Int deriving (Eq, Ord)

newtype Column = Column Int deriving (Eq, Ord)

newtype Spot = Spot (Row, Column) deriving (Eq, Ord)

data Face = FaceUp | FaceDown deriving (Eq)

newtype CardName = CardName String

data Card = Card CardName Face

data Grid = Grid {cells :: Map.Map Spot Card, atlas :: Atlas}

instance Render.Render Grid where
  render Grid {cells, atlas} tr =
    mconcat (vert <$> Map.toList cells)
    where
      padding = 10
      faceDown = Atlas.sprite atlas "back-side"
      WithVec w h = faceDown.resolution
      vert (Spot (Row r, Column c), crd) = Render.render (card crd) (tr <> base r c <> pivot)
      c = 6
      r = 6
      pivot = let f n = n * (h + padding) - padding in Affine.origin (f c / 2) (f r / 2)
      base r c = Affine.translate (fromIntegral c * (w + padding)) (fromIntegral r * (h + padding))
      card (Card (CardName name) FaceUp) = Atlas.sprite atlas name
      card (Card _ FaceDown) = faceDown

data World = World
  { pointer :: PixelVec,
    atlas :: Atlas,
    font :: Atlas,
    grid :: In Grid WorldVec,
    cardSize :: WorldVec,
    camera :: Cam.Camera,
    pressedKeys :: Set.Set SDL.Scancode
  }

mkSuffeledDeck :: Int -> IO (V.Vector CardName)
mkSuffeledDeck n = do
  g <- Random.initStdGen
  v <- MV.generate (2 * n) (\i -> (i `mod` n) + 1)
  deck <- shuffle g v
  return $ CardName . show <$> deck

shuffle :: Random.StdGen -> MV.MVector (MV.PrimState IO) Int -> IO (V.Vector Int)
shuffle g0 init = do
  out <- go g0 (MV.length init) init
  V.freeze out
  where
    go _ n v | n <= 1 = return v
    go g n v = do
      let (i, g') = Random.uniformR (0, n - 2) g
      MV.swap v i (n - 1)
      go g' (n - 1) v

mkGrid :: [CardName] -> Atlas -> Grid
mkGrid deck atlas = Grid {cells = Map.fromList $ zipWith f spots deck, atlas = atlas}
  where
    spots = [Spot (Row r, Column c) | r <- [0 .. 5], c <- [0 .. 5]]
    f spot name = (spot, Card name FaceDown)

world0 :: (MonadIO io) => Atlas -> Atlas -> io World
world0 atlas font = do
  deck <- liftIO $ mkSuffeledDeck 18
  return $
    World
      { pointer = vec 0 0,
        atlas = atlas,
        font = font,
        grid = putIn (mkGrid (V.toList deck) atlas) (vec 0 0),
        cardSize = vec 1.0 1.0,
        camera = Cam.defaultCamera,
        pressedKeys = Set.empty
      }

data FrameData = FrameData {verts :: SV.Vector Vert.Vertex}

frame ::
  (MonadIO io) =>
  Vk.Device ->
  Vk.Queue ->
  Vk.Queue ->
  Vk.Pipeline ->
  Vk.PipelineLayout ->
  (Vk.SwapchainKHR, VkExtent2D.Extent2D, V.Vector Vk.Image) ->
  Vk.DescriptorSet ->
  Frame ->
  FrameData ->
  io ()
frame device gfx present pipeline pipelineLayout swp descSet f frameData = do
  let Frame
        { pool,
          renderCmd,
          copyCmd,
          fence,
          imageAvailable,
          renderFinished,
          copyFinished,
          staging,
          vertex,
          targetImage,
          targetView
        } = f
      FrameData {verts} = frameData
      (swapchain, swapchainExtent, swapchainImages) = swp
  waitForFrame device f
  liftIO $ Utils.copyToGpu device pool gfx vertex staging verts
  recordRender renderCmd vertex (targetImage, targetView) swapchainExtent (fromIntegral $ SV.length verts)
  index <- Vk.acquireNextImageKHR device swapchain maxBound imageAvailable Vk.zero >>= \(r, index) -> checkSwapchainIsOld r $> index
  let swapchainImage = swapchainImages ! fromIntegral index
   in recordCopyToSwapchain copyCmd targetImage swapchainImage swapchainExtent

  let render =
        Vk.SomeStruct
          Vk.zero
            { VkSubmitInfo.commandBuffers = [Vk.commandBufferHandle renderCmd],
              VkSubmitInfo.signalSemaphores = [renderFinished]
            }
      copy =
        Vk.SomeStruct
          Vk.zero
            { VkSubmitInfo.waitSemaphores = [imageAvailable, renderFinished],
              VkSubmitInfo.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT, Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT],
              VkSubmitInfo.commandBuffers = [Vk.commandBufferHandle copyCmd],
              VkSubmitInfo.signalSemaphores = [copyFinished]
            }
   in Vk.queueSubmit gfx [render, copy] fence
  let info =
        Vk.zero
          { VkPresentInfoKHR.waitSemaphores = [copyFinished],
            VkPresentInfoKHR.swapchains = [swapchain],
            VkPresentInfoKHR.imageIndices = [index]
          }
   in Vk.queuePresentKHR present info >>= checkSwapchainIsOld
  where
    checkSwapchainIsOld r = when (r == Vk.SUBOPTIMAL_KHR || r == Vk.ERROR_OUT_OF_DATE_KHR) (say "Engine" $ "presentFrame" ++ show r)
    recordCopyToSwapchain cmd offscreenImage swapchainImage extent =
      Vk.useCommandBuffer cmd Vk.zero $ do
        transitRenderTargetToCopySrc cmd offscreenImage
        transitToCopyDst cmd swapchainImage
        copyImageToImage cmd offscreenImage swapchainImage extent
        transitToPresent cmd swapchainImage
    recordRender cmd vertBuff target extent vertexCount =
      Vk.useCommandBuffer cmd Vk.zero $ do
        let (image, view) = target
        Vk.cmdBindPipeline cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
        Vk.cmdBindVertexBuffers cmd 0 [vertBuff] [0]
        Vk.cmdBindDescriptorSets cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 [descSet] []
        transitToRenderTarget cmd image
        let attachment =
              Vk.zero
                { VkRenderingAttachmentInfo.imageView = view,
                  VkRenderingAttachmentInfo.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                  VkRenderingAttachmentInfo.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
                  VkRenderingAttachmentInfo.storeOp = Vk.ATTACHMENT_STORE_OP_STORE,
                  VkRenderingAttachmentInfo.clearValue = clearColor
                }
            scissor = Vk.Rect2D {VkRect2D.offset = Vk.Offset2D 0 0, VkRect2D.extent = extent}
            info =
              Vk.zero
                { VkRenderingInfo.renderArea = scissor,
                  VkRenderingInfo.layerCount = 1,
                  VkRenderingInfo.colorAttachments = [attachment]
                }
            draw = Vk.cmdDraw cmd vertexCount 1 0 0
         in Vk.cmdUseRendering cmd info draw

withFrames ::
  VkDevice.Device ->
  Word32 ->
  Vma.Allocator ->
  Vk.DeviceSize ->
  Vk.DeviceSize ->
  Int ->
  Managed (V.Vector Frame)
withFrames device gfx allocator stagingBufferSize vertexBufferSize frameCount = V.replicateM frameCount singleFrame
  where
    singleFrame =
      do
        pool <-
          let info =
                Vk.zero
                  { VkCommandPoolCreateInfo.queueFamilyIndex = gfx,
                    VkCommandPoolCreateInfo.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                  }
           in managed $ Vk.withCommandPool device info Nothing bracket
        cmds <-
          let info =
                Vk.zero
                  { Vk.commandPool = pool,
                    Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
                    Vk.commandBufferCount = 2
                  }
           in managed (Vk.withCommandBuffers device info bracket)
        vertextStagingBuffer <- Tex.withHostBuffer allocator stagingBufferSize
        imageAvailable <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
        renderFinished <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
        copyFinished <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
        fence <-
          let info = Vk.zero {VkFenceCreateInfo.flags = Vk.FENCE_CREATE_SIGNALED_BIT}
           in managed $ Vk.withFence device info Nothing bracket
        vertexBuffer <- withGPUBuffer allocator vertexBufferSize Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
        let WithVec w h = windowSize
        (image, view) <- Tex.withImageAndView allocator device (vec (fromIntegral w) (fromIntegral h)) Init.imageFormat
        return
          Frame
            { pool = pool,
              renderCmd = cmds ! 0,
              copyCmd = cmds ! 1,
              fence = fence,
              imageAvailable = imageAvailable,
              renderFinished = renderFinished,
              copyFinished = copyFinished,
              staging = vertextStagingBuffer,
              vertex = vertexBuffer,
              targetImage = image,
              targetView = view
            }

waitForFrame :: (MonadIO io) => Vk.Device -> Frame -> io ()
waitForFrame device (Frame {fence}) =
  let second = 1000000000 in Vk.waitForFences device [fence] True second *> Vk.resetFences device [fence]

mainLoop ::
  (MonadIO io) =>
  io () ->
  (w -> FrameData) ->
  (Int -> FrameData -> io ()) ->
  (TimeSeconds -> w -> io w) ->
  (SDL.Event -> w -> io w) ->
  w ->
  io ()
mainLoop shutdown frameData frame worldTime worldEvent w0 =
  do
    t0 <- SDL.ticks
    go 0 t0 w0
  where
    lockFrameRate fps t1 =
      do
        t <- SDL.ticks
        let dt = t - t1
            minIdle = 1000 `div` fps
        if dt < minIdle then (liftIO . threadDelay $ 1000 * fromIntegral (minIdle - dt)) *> SDL.ticks else pure t
    go n t w = do
      es <- SDL.pollEvents
      if any isQuitEvent es
        then shutdown
        else do
          frame n $ frameData w
          w2 <- foldlM (flip worldEvent) w es
          t2 <- lockFrameRate 120 t
          w3 <- let dt = fromIntegral (t2 - t) in worldTime (TimeSeconds (dt / 1000)) w2
          go (n + 1) t2 w3

worldEvent :: (Monad io) => SDL.Event -> World -> io World
worldEvent e w@(World {grid, pointer, cardSize, camera, pressedKeys}) =
  return
    w
      { grid = grid {object = grid'},
        pointer = pointer',
        pressedKeys = pressedKeys',
        camera = camera'
      }
  where
    grid' = if mouseClicked then flip else grid.object
    camera'
      | keyReleasedIs SDL.Scancode0 = Cam.defaultCamera
      | otherwise = camera
    pressedKeys'
      | Just code <- keyPressed = Set.insert code pressedKeys
      | Just code <- keyReleased = Set.delete code pressedKeys
      | otherwise = pressedKeys
    pointer' = fromMaybe w.pointer mouseMoved
    flip
      | Just spot <- spot = gridFlip spot grid.object
      | otherwise = grid.object
    spot =
      let WithVec x y = tr (screenToWorld windowSize ppu camera) pointer :: WorldVec
          WithVec px py = vec @WorldVec 0 0
          WithVec w h = cardSize
          r = floor $ (y + 1) / (h + px)
          c = floor $ (x + 1) / (w + py)
       in Just (Spot (Row r, Column c))
    mouseClicked
      | SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Released _ SDL.ButtonLeft _ _)) <- e = True
      | otherwise = False
    mouseMoved
      | (SDL.Event _ (SDL.MouseMotionEvent (SDL.MouseMotionEventData {mouseMotionEventPos = SDL.P (SDL.V2 x y)}))) <- e = Just $ vec (fromIntegral x) (fromIntegral y)
      | otherwise = Nothing
    keyPressed
      | (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData {keyboardEventKeyMotion = SDL.Pressed, keyboardEventKeysym = SDL.Keysym {keysymScancode = code}}))) <- e = Just code
      | otherwise = Nothing
    keyReleased
      | (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData {keyboardEventKeyMotion = SDL.Released, keyboardEventKeysym = SDL.Keysym {keysymScancode = code}}))) <- e = Just code
      | otherwise = Nothing
    keyReleasedIs code = Just code == keyReleased

gridFlip :: Spot -> Grid -> Grid
gridFlip spot grid = grid {cells = Map.adjust cardFlip spot grid.cells}

cardFlip :: Card -> Card
cardFlip (Card name FaceUp) = Card name FaceDown
cardFlip (Card name FaceDown) = Card name FaceUp

worldTime :: (Monad io) => TimeSeconds -> World -> io World
worldTime (TimeSeconds dt) w = return $ w {camera = foldl (\cam act -> act cam) w.camera cameraActions}
  where
    cameraActions =
      [Cam.moveUp $ moveSpeed * dt | Set.member SDL.ScancodeUp w.pressedKeys]
        ++ [Cam.moveDown $ moveSpeed * dt | Set.member SDL.ScancodeDown w.pressedKeys]
        ++ [Cam.moveLeft $ moveSpeed * dt | Set.member SDL.ScancodeLeft w.pressedKeys]
        ++ [Cam.moveRight $ moveSpeed * dt | Set.member SDL.ScancodeRight w.pressedKeys]
        ++ [Cam.rotateCcw $ rotationSpeed * dt | Set.member SDL.ScancodeE w.pressedKeys]
        ++ [Cam.rotateCw $ rotationSpeed * dt | Set.member SDL.ScancodeR w.pressedKeys]
        ++ [Cam.zoomIn $ zoomStep * dt | Set.member SDL.ScancodeEquals w.pressedKeys]
        ++ [Cam.zoomOut $ zoomStep * dt | Set.member SDL.ScancodeMinus w.pressedKeys, w.camera.zoom >= minZoom]
    moveSpeed = 5 -- World units / s
    zoomStep = 1 -- 1x / s
    rotationSpeed = 0.5 * pi -- Rad / s
    minZoom = 0.00

windowSize :: ViewportSize
windowSize = vec 900 900

ppu :: PPU
ppu = Measure.ppu 100

screenToWorld :: ViewportSize -> PPU -> Cam.Camera -> Affine
screenToWorld vps@(WithVec w h) ppu cam = ndc2World <> pixels2Ndc
  where
    ndc2World = Affine.inverse (projection vps ppu <> Cam.view cam)
    pixels2Ndc = srt (s w, s h) 0 (-1, -1)
    s x = 2 / fromIntegral x

scene :: World -> [Object]
scene World {pointer, atlas, grid, font} = inWorld grid : (worldText ++ screenR)
  where
    str = "This is a sample text 0123456789!@#$%^&*()_+[]{}\";;?><,.~`"
    worldText =
      [ inWorld $ let txt = putIn (let x = text "Pivoted and then Rotated 45 degrees" (Vert.opaqueColor 0.0 0.0 0.0) font in x {Txt.origin = vec 30 100}) (vec @WorldVec 0 0) in setRotation (rotateDegree 45) txt,
        inWorld $ let txt = putIn (text "Scaled diffirently on X and Y" (Vert.opaqueColor 0.0 0.0 0.0) font) (vec @WorldVec 1 1) in setScale (scaleXY 0.7 1.5) txt,
        inWorld $ putIn (text "Colored" (Vert.opaqueColor 1.0 1.0 0.0) font) (vec @WorldVec 0 2),
        inWorld $ putIn (text str (Vert.opaqueColor 0.0 0.0 0.0) font) (vec @WorldVec 0 0)
      ]
    screenR =
      [ inScreen $ srtPutIn r0 noScale (rotateDegree 45) (vec 0 0 :: PixelVec) (vec 0 0),
        inScreen $ srtPutIn r1 noScale (rotateDegree (-30)) (vec sw 0 :: PixelVec) (vec 100 0),
        inScreen $ srtPutIn r2 noScale (rotateDegree 30) (vec sw sh :: PixelVec) (vec 100 50),
        inScreen $ srtPutIn r3 (scaleXY 0.5 2) (rotateDegree 20) (vec 0 sh :: PixelVec) (vec 0 50),
        inScreen $ srtPutIn r4 (scaleXY 2 0.5) (rotateDegree 20) (vec (sw / 2) (sh / 2) :: PixelVec) (vec 50 25),
        inScreen txt1,
        inScreen txt2,
        inScreen txt3,
        inScreen txt4,
        inScreen $ putIn (Atlas.sprite atlas "pointer") pointer
      ]
    WithVec _w _h = windowSize
    sw = fromIntegral _w
    sh = fromIntegral _h
    WithVec rw rh = vec 100 50 :: PixelVec
    r0 = rect 0
    r1 = rect 1
    r2 = rect 2
    r3 = rect 3
    r4 = rect 4
    rect = Atlas.spriteIndexed atlas "rectangle"
    y0 line = let y = line * 16 in vec 30 (30 + y) :: PixelVec
    txt1 = putIn (text "Move the camera: Arrow keys" (Vert.opaqueColor 1.0 1.0 1.0) font) (y0 0)
    txt2 = putIn (text "Rotate: E and R " (Vert.opaqueColor 1.0 0.0 0.0) font) (y0 1)
    txt3 = putIn (text "Zoom in and out: + and -" (Vert.opaqueColor 0.0 1.0 0.0) font) (y0 2)
    txt4 = putIn (text "Reset: 0" (Vert.opaqueColor 0.0 0.0 1.0) font) (y0 3)

class Has game a where
  get :: game -> a

newtype Font = Font Atlas

data Game = Game
  { windowSize :: ViewportSize,
    font :: Font,
    camera :: Cam.Camera,
    ppu :: PPU,
    atlas :: Atlas
  }

instance Has Game ViewportSize where
  get = (.windowSize)

-- TODO both font and atlas are Atlas. Probably new newtype wrapper or rethink the pattern
instance Has Game Atlas where
  get = (.atlas)

instance Has Game Font where
  get = (.font)

instance Has Game Cam.Camera where -- TODO camera can change
  get = (.camera)

instance Has Game PPU where
  get = (.ppu)

data WS = W | S

data Object = forall obj. (Render.Render obj) => Object obj WS

inWorld :: (Render.Render obj) => obj -> Object
inWorld obj = Object obj W

inScreen :: (Render.Render obj) => obj -> Object
inScreen obj = Object obj S

class Render game where
  world :: game -> Affine
  screen :: game -> Affine

instance (Has game Cam.Camera, Has game PPU, Has game ViewportSize) => Render game where
  world game = Render.world (get game) (get game) (get game)
  screen game = Render.screen (get game)

clearColor :: Vk.ClearValue
clearColor = Vk.Color (Vk.Float32 0.1 0.1 0.2 0)