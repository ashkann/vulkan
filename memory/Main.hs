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
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Affine (Affine, applyVec, inverse, noRotation, noScale, originXY, rotateDegree, scaleXY, srt, srt3, translateXY, uniformScale, translate)
import Atlas (Atlas)
import qualified Atlas
import qualified Camera as Cam
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Managed (Managed, MonadIO (liftIO), managed, runManaged)
import Data.Functor (($>))
import Data.List (foldl')
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
import Measure (PixelVec, Vec (Vec), ViewportSize (ViewportSize), WorldVec, mkWindowSize, vec, NDC)
import Node (Tree, node, tree, tree0)
import qualified Render
import qualified SDL
import Sprite
import qualified System.Random as Random
import qualified Texture as Tex
import Txt (text)
import Update (TimeSeconds (..), Update (..), timeSecondsFromMillis)
import Utils
import Vertex (Vertex)
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
import World (PPU, pixelToWorld)
import qualified World
import Prelude hiding (init)

data VulkanFrame = VulkanFrame
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
  let Just windowSize = mkWindowSize 900 900
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
      vertexBufferSize = fromIntegral $ sizeOf (undefined :: Vert.Vertex NDC) * maxVertCount
  frames <- withFrames device gfx.index allocator stagingBufferSize vertexBufferSize frameCount windowSize
  w0 <- liftIO $ world0 atlas font
  let shutdown = say "Engine" "Shutting down ..." *> Vk.deviceWaitIdle device
      game w = Game {windowSize = w.windowSize, font = Font font, camera = w.camera, ppu = w.ppu, atlas = atlas} -- TODO move 'World' into 'Game'
   in say "Engine" "Entering the main loop"
        *> mainLoop
          shutdown
          (\w -> frameData (game w) w)
          (\n d -> let f = frames ! (n `mod` frameCount) in frame device gfxQueue presentQueue pipeline pipelineLayout swapchain descSet f d)
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

toTree :: Grid -> Tree
toTree Grid {cells, atlas} = tree0 (f <$> Map.toList cells)
  where
    padding = 10
    faceDown = Atlas.sprite atlas "back-side"
    Vec w h = faceDown.resolution
    f (Spot (Row r, Column c), crd) = node (base r c <> pivot) (card crd)
    c = 6
    r = 6
    pivot = let f n = n * (h + padding) - padding in originXY (f c / 2) (f r / 2)
    base r c = translateXY (fromIntegral c * (w + padding)) (fromIntegral r * (h + padding)) -- use world units ?
    card (Card (CardName name) FaceUp) = Atlas.sprite atlas name
    card (Card _ FaceDown) = faceDown

data World = World
  { pointer :: PixelVec,
    atlas :: Atlas,
    font :: Atlas,
    grid :: Grid,
    cardSize :: WorldVec,
    pressedKeys :: Set.Set SDL.Scancode,
    camera :: Cam.Camera,
    ppu :: World.PPU,
    windowSize :: ViewportSize
  }

instance Update World where
  event = foldl' f
    where
      f w@(World {grid, pointer, cardSize, camera, ppu, windowSize}) e = w {pressedKeys = update w.pressedKeys, grid = grid', pointer = pointer'}
        where
          grid' = if mouseClicked then flip else grid
          pointer' = fromMaybe w.pointer mouseMoved
          flip
            | Just spot <- spot = gridFlip spot grid
            | otherwise = grid
          spot =
            let Vec x y = applyVec (screenToWorld windowSize ppu camera) pointer -- :: WorldVec
                Vec px py = vec @WorldVec 0 0
                Vec w h = cardSize
                r = floor $ (y + 1) / (h + px)
                c = floor $ (x + 1) / (w + py)
             in Just (Spot (Row r, Column c))
          mouseClicked
            | SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Released _ SDL.ButtonLeft _ _)) <- e = True
            | otherwise = False
          mouseMoved
            | (SDL.Event _ (SDL.MouseMotionEvent (SDL.MouseMotionEventData {mouseMotionEventPos = SDL.P (SDL.V2 x y)}))) <- e = Just $ vec (fromIntegral x) (fromIntegral y)
            | otherwise = Nothing
          update ks
            | Just code <- pressed = Set.insert code ks
            | Just code <- released = Set.delete code ks
            | otherwise = ks
          pressed
            | Just (SDL.Pressed, code) <- kbd = Just code
            | otherwise = Nothing
          released
            | Just (SDL.Released, code) <- kbd = Just code
            | otherwise = Nothing
          kbd
            | (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData {SDL.keyboardEventKeyMotion = m, SDL.keyboardEventKeysym = SDL.Keysym {SDL.keysymScancode = code}}))) <- e = Just (m, code)
            | otherwise = Nothing

  time w (TimeSeconds dt) = w {camera = camera'}
    where
      camera' = foldl' (\cam key -> let f = fromMaybe id (Map.lookup key map) in f cam) w.camera w.pressedKeys
      moveSpeed = 5 -- World units / s
      zoomStep = 1 -- 1x / s
      rotationSpeed = 0.5 * pi -- Rad / s
      minZoom = 0.00
      map =
        Map.fromList
          [ (SDL.Scancode0, const Cam.defaultCamera),
            (SDL.ScancodeUp, Cam.moveUp $ moveSpeed * dt),
            (SDL.ScancodeDown, Cam.moveDown $ moveSpeed * dt),
            (SDL.ScancodeLeft, Cam.moveLeft $ moveSpeed * dt),
            (SDL.ScancodeRight, Cam.moveRight $ moveSpeed * dt),
            (SDL.ScancodeQ, Cam.rotateCcw $ rotationSpeed * dt),
            (SDL.ScancodeE, Cam.rotateCw $ rotationSpeed * dt),
            (SDL.ScancodeEquals, Cam.zoomIn $ zoomStep * dt),
            (SDL.ScancodeMinus, if w.camera.zoom >= minZoom then Cam.zoomOut $ zoomStep * dt else id)
          ]

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
        grid = mkGrid (V.toList deck) atlas,
        cardSize = vec 1.0 1.0,
        camera = Cam.defaultCamera,
        pressedKeys = Set.empty,
        ppu = World.ppu 100,
        windowSize = ws
      }
  where
    Just ws = mkWindowSize 900 900

data Frame = Frame {verts :: SV.Vector (Vertex NDC)}

frameData :: Game -> World -> Frame
frameData (Game {camera, windowSize, ppu}) w =
  Frame
    { verts = SV.fromList $ Render.applyObject mempty frame
    }
  where
    frame = tree0 [node worldTr world, node screenTr screen]
    (world, screen) = scene w
    worldTr = World.world windowSize ppu camera
    screenTr = World.screen windowSize

frame ::
  (MonadIO io) =>
  Vk.Device ->
  Vk.Queue ->
  Vk.Queue ->
  Vk.Pipeline ->
  Vk.PipelineLayout ->
  (Vk.SwapchainKHR, VkExtent2D.Extent2D, V.Vector Vk.Image) ->
  Vk.DescriptorSet ->
  VulkanFrame ->
  Frame ->
  io ()
frame device gfx present pipeline pipelineLayout swp descSet f frameData = do
  let VulkanFrame
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
      Frame {verts} = frameData
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
  ViewportSize ->
  Managed (V.Vector VulkanFrame)
withFrames device gfx allocator stagingBufferSize vertexBufferSize frameCount vps = V.replicateM frameCount singleFrame
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
        let ViewportSize w h = vps
        (image, view) <- Tex.withImageAndView allocator device (vec (fromIntegral w) (fromIntegral h)) Init.imageFormat
        return
          VulkanFrame
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

waitForFrame :: (MonadIO io) => Vk.Device -> VulkanFrame -> io ()
waitForFrame device (VulkanFrame {fence}) =
  let second = 1000000000 in Vk.waitForFences device [fence] True second *> Vk.resetFences device [fence]

mainLoop ::
  (MonadIO io, Update world) =>
  io () ->
  (world -> Frame) ->
  (Int -> Frame -> io ()) ->
  world ->
  io ()
mainLoop shutdown frame render w0 =
  do
    t0 <- SDL.ticks
    go 0 t0 w0
  where
    go n t w = do
      es <- SDL.pollEvents
      if any isQuitEvent es
        then shutdown
        else do
          render n $ frame w
          let w2 = event w es
          t2 <- lockFrameRate 120 t
          let dt = timeSecondsFromMillis t2 t
              w3 = time w2 dt
          go (n + 1) t2 w3
    lockFrameRate fps t1 =
      do
        t <- SDL.ticks
        let dt = t - t1
            minIdle = 1000 `div` fps
        if dt < minIdle then (liftIO . threadDelay $ 1000 * fromIntegral (minIdle - dt)) *> SDL.ticks else pure t

gridFlip :: Spot -> Grid -> Grid
gridFlip spot grid = grid {cells = Map.adjust cardFlip spot grid.cells}

cardFlip :: Card -> Card
cardFlip (Card name FaceUp) = Card name FaceDown
cardFlip (Card name FaceDown) = Card name FaceUp

screenToWorld :: ViewportSize -> PPU -> Cam.Camera -> Affine
screenToWorld vps@(ViewportSize w h) ppu cam = ndc2World <> pixels2Ndc
  where
    ndc2World = Affine.inverse (World.projection vps ppu <> Cam.view cam)
    pixels2Ndc = srt3 (s w, s h) 0 (-1, -1)
    s x = 2 / fromIntegral x

scene :: World -> (Tree, Tree)
scene World {pointer, atlas, grid, font, windowSize, ppu} = (tree (pixelToWorld ppu) $ toTree grid : worldText, tree0 [screen2, ptr])
  where
    ptr = node (Affine.translate pointer) (Atlas.sprite atlas "pointer")
    screen2 = tree (Affine.srt (uniformScale 0.5) (rotateDegree 30) (vec 300 300 :: WorldVec) (vec 450 450 :: PixelVec)) screenR
    rect = Atlas.spriteIndexed atlas "rectangle"
    r12 =
      tree0
        [ node (Affine.srt noScale noRotation (vec 1 0 :: PixelVec) (vec 0 0 :: PixelVec)) (rect 0) -- ,
        -- node (rect 1) $ Affine.srt noScale noRotation (vec 100 50 :: PixelVec) (vec 0 0 :: PixelVec)
        ]
    -- \$ Affine.srt noScale noRotation (vec 100 100 :: WorldVec) (vec 0 0 :: PixelVec)

    str = "This is a sample text 0123456789!@#$%^&*()_+[]{}\";;?><,.~`"
    worldText =
      [ node (Affine.srt noScale (rotateDegree 45) (vec 0 0 :: WorldVec) (vec 30 100 :: PixelVec)) (text "Pivoted and then Rotated 45 degrees" (color 0.0 0.0 0.0) font),
        node (Affine.srt (scaleXY 0.7 1.5) noRotation (vec 0 0 :: WorldVec) (vec 30 100 :: PixelVec)) (text "Scaled diffirently on X and Y" (color 0.0 0.0 0.0) font),
        node (Affine.srt noScale noRotation (vec 0 0 :: WorldVec) (vec 0 0 :: PixelVec)) (text "Colored" (color 1.0 1.0 0.0) font),
        node (Affine.srt noScale noRotation (vec 0 0 :: WorldVec) (vec 0 0 :: PixelVec)) (text str (color 0.0 0.0 0.0) font)
      ]
    screenR =
      [ node (Affine.srt noScale (rotateDegree 45) (vec 0 0 :: PixelVec) (vec 0 0 :: PixelVec)) r1,
        node (Affine.srt noScale (rotateDegree (-30)) (vec sw 0 :: PixelVec) (vec 100 0 :: PixelVec)) r2,
        node (Affine.srt noScale (rotateDegree 30) (vec sw sh :: PixelVec) (vec 100 50 :: PixelVec)) r3,
        node (Affine.srt (scaleXY 0.5 2) (rotateDegree 20) (vec 0 sh :: PixelVec) (vec 0 50 :: PixelVec)) r4,
        node (Affine.srt (scaleXY 2 0.5) (rotateDegree 20) (vec (sw / 2) (sh / 2) :: PixelVec) (vec 50 25 :: PixelVec)) r5,
        node (t (y0 0)) (text "Move the camera: Arrow keys" (color 1.0 1.0 1.0) font),
        node (t (y0 1)) (text "Rotate: E and R " (color 1.0 0.0 0.0) font),
        node (t (y0 2)) (text "Zoom in and out: + and -" (color 0.0 1.0 0.0) font),
        node (t (y0 3)) (text "Reset: 0" (color 0.0 0.0 1.0) font)
      ]
      where
        ViewportSize _w _h = windowSize
        sw = fromIntegral _w
        sh = fromIntegral _h
        Vec rw rh = vec 100 50 :: PixelVec
        r1 = rect 0
        r2 = rect 1
        r3 = rect 2
        r4 = rect 3
        r5 = rect 4
        rect = Atlas.spriteIndexed atlas "rectangle"
        y0 line = let y = line * 16 in vec 30 (30 + y) :: PixelVec
        t = Affine.translate
    color = Vert.opaqueColor

newtype Font = Font Atlas

data Game = Game
  { windowSize :: ViewportSize,
    font :: Font,
    camera :: Cam.Camera,
    ppu :: PPU,
    atlas :: Atlas
  }

clearColor :: Vk.ClearValue
clearColor = Vk.Color (Vk.Float32 0.1 0.1 0.2 0)