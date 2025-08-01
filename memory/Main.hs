{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import qualified Atlas
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Managed (Managed, MonadIO (liftIO), managed, runManaged)
import Data.Bits ((.|.))
import Data.Foldable (foldlM)
import Data.Functor (($>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as SV
import Foreign (Ptr, Storable, Word32, Word64)
import Foreign.Storable (Storable (..), sizeOf)
import qualified Foreign.Storable.Record as Store
import qualified Geomancy as G
import qualified Init
import Measure
import qualified SDL
import SRT (SRT, srt)
import qualified SRT
import qualified System.Random as Random
import Texture (putInWorld, rotateSprite)
import qualified Texture as Tex
import Utils
import qualified Vertex as Vert
import qualified Vulkan as Vk
import qualified Vulkan as VkCommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan as VkDescriptorBufferInfo (DescriptorBufferInfo (..))
import qualified Vulkan as VkDescriptorPoolCreateInfo (DescriptorPoolCreateInfo (..))
import qualified Vulkan as VkDescriptorPoolSize (DescriptorPoolSize (..))
import qualified Vulkan as VkDescriptorSetAllocateInfo (DescriptorSetAllocateInfo (..))
import qualified Vulkan as VkDescriptorSetLayoutBinding (DescriptorSetLayoutBinding (..))
import qualified Vulkan as VkDescriptorSetLayoutBindingFlagsCreateInfo (DescriptorSetLayoutBindingFlagsCreateInfo (..))
import qualified Vulkan as VkDescriptorSetLayoutCreateInfo (DescriptorSetLayoutCreateInfo (..))
import qualified Vulkan as VkDevice (Device (..))
import qualified Vulkan as VkExtent2D (Extent2D (..))
import qualified Vulkan as VkFenceCreateInfo (FenceCreateInfo (..))
import qualified Vulkan as VkGraphicsPipelineCreateInfo (GraphicsPipelineCreateInfo (..))
import qualified Vulkan as VkPipelineColorBlendStateCreateInfo (PipelineColorBlendStateCreateInfo (..))
import qualified Vulkan as VkPipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan as VkPipelineRenderingCreateInfo (PipelineRenderingCreateInfo (..))
import qualified Vulkan as VkPipelineVertexInputStateCreateInfo (PipelineVertexInputStateCreateInfo (..))
import qualified Vulkan as VkPresentInfoKHR (PresentInfoKHR (..))
import qualified Vulkan as VkRect2D (Rect2D (..))
import qualified Vulkan as VkRenderingAttachmentInfo (RenderingAttachmentInfo (..))
import qualified Vulkan as VkRenderingInfo (RenderingInfo (..))
import qualified Vulkan as VkSubmitInfo (SubmitInfo (..))
import qualified Vulkan as VkVPipelineMultisampleStateCreateInfo (PipelineMultisampleStateCreateInfo (..))
import qualified Vulkan as VkVertexInputAttributeDescription (VertexInputAttributeDescription (..))
import qualified Vulkan as VkVertexInputBindingDescription (VertexInputBindingDescription (..))
import qualified Vulkan as VkViewport (Viewport (..))
import qualified Vulkan as VkWriteDescriptorSet (WriteDescriptorSet (..))
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified VulkanMemoryAllocator as Vma
import Prelude hiding (init)

newtype TimeSeconds = TimeSeconds Float

data Viewport = Viewport
  { viewportSize :: G.UVec2,
    _padding :: Word64 -- TODO: remove and make the Storable instance handle it
  }

viewportStore :: Store.Dictionary Viewport
viewportStore =
  Store.run $
    Viewport
      <$> Store.element (.viewportSize)
      <*> Store.element (const 0)

instance Storable Viewport where
  sizeOf = Store.sizeOf viewportStore
  alignment = Store.alignment viewportStore
  peek = Store.peek viewportStore
  poke = Store.poke viewportStore

frameData :: World -> FrameData
frameData world =
  FrameData
    { verts =
        let worldVerts = mconcat $ vertices world.camera <$> sprites world
            screenVerts = mconcat $ screenVertices windowSize <$> screenSprites world
         in worldVerts SV.++ screenVerts,
      viewport = Viewport {viewportSize = let WithVec w h = windowSize in G.uvec2 w h, _padding = 0} -- TODO: remove _padding = 0
    }

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
    viewport :: Vk.Buffer,
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
  descSetLayout <- descriptorSetLayout device descriptorCount
  descPool <- descriptorPool device 1000
  descSet <- descriptorSet device descSetLayout descPool
  sampler <- Utils.repeatingSampler device
  gfxQueue <- Vk.getDeviceQueue device gfx.index 0 <* say "Vulkan" "Got graphics queue"

  atlas <- Atlas.withAtlas allocator device commandPool gfxQueue descSet sampler "out/memory"
  say "Engine" "Atlas loaded"

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
  pipeline <- createPipeline device swapchainExtent pipelineLayout
  presentQueue <- Vk.getDeviceQueue device present.index 0 <* say "Vulkan" "Got present queue"

  let stagingBufferSize = 1048576
      maxVertCount = 1000
      vertexBufferSize = fromIntegral $ sizeOf (undefined :: Vert.Vertex) * maxVertCount
      lightBufferSize = 1024
  frames <- withFrames device gfx.index allocator stagingBufferSize vertexBufferSize lightBufferSize frameCount
  w0 <- liftIO $ world0 atlas
  let shutdown = say "Engine" "Shutting down ..." *> Vk.deviceWaitIdle device
   in say "Engine" "Entering the main loop"
        *> mainLoop
          shutdown
          frameData
          ( \frameNumber frameData ->
              do
                let index = frameNumber `mod` frameCount
                    f = frames ! index
                 in frame device gfxQueue presentQueue pipeline pipelineLayout swapchain descSet f frameData
          )
          worldTime
          worldEvent
          w0

-- data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Eq, Ord, Enum)

-- newtype Score = Score (Digit, Digit, Digit)

-- mkScore :: Int -> Maybe Score
-- mkScore n
--   | n >= 0 && n <= 999 = Just $ Score (toEnum $ n `div` 100, toEnum $ (n `div` 10) `mod` 10, toEnum $ n `mod` 10)
--   | otherwise = Nothing

-- ashkan :: Atlas.Atlas -> Int -> [Object]
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

newtype Grid = Grid (Map.Map Spot Card)

data GridStuff = GridStuff
  { topLeft :: WorldVec,
    padding :: WorldVec
  }

data World = World
  { pointer :: PixelVec,
    atlas :: Atlas.Atlas,
    grid :: Grid,
    gridStuff :: GridStuff,
    cardSize :: WorldVec,
    camera :: Camera,
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

mkGrid :: [CardName] -> Grid
mkGrid deck = Grid . Map.fromList $ zipWith f spots deck
  where
    spots = [Spot (Row r, Column c) | r <- [0 .. 5], c <- [0 .. 0]]
    f spot name = (spot, Card name FaceDown)

world0 :: (MonadIO io) => Atlas.Atlas -> io World
world0 atlas = do
  deck <- liftIO $ mkSuffeledDeck 18
  let faceDown = Tex.putInWorld (Atlas.sprite atlas "back-side")
   in return $
        World
          { pointer = vec 0 0,
            atlas = atlas,
            grid = mkGrid (V.toList deck),
            gridStuff =
              GridStuff
                { topLeft = vec 0.5 0.5,
                  padding = vec 0.2 0.2
                },
            cardSize = vec 1.0 1.0,
            camera = Camera {position = vec 0 0, rotation = 0, zoom = 1.0},
            pressedKeys = Set.empty
          }

data FrameData = FrameData
  { verts :: SV.Vector Vert.Vertex,
    viewport :: Viewport
  }

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
          viewport,
          targetImage,
          targetView
        } = f
      FrameData {verts, viewport = viewport'} = frameData
      (swapchain, swapchainExtent, swapchainImages) = swp
  waitForFrame device f
  liftIO $ Utils.copyToGpu2 device pool gfx viewport staging viewport'
  liftIO $ Utils.copyToGpu device pool gfx vertex staging verts
  -- say "Global light" $ show viewport'.globalLight
  recordRender renderCmd vertex viewport (targetImage, targetView) swapchainExtent (fromIntegral $ SV.length verts)
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
    recordRender cmd vertBuff viewportBuff target extent vertexCount =
      Vk.useCommandBuffer cmd Vk.zero $ do
        let (image, view) = target
        Vk.cmdBindPipeline cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
        Vk.cmdBindVertexBuffers cmd 0 [vertBuff] [0]
        Vk.cmdBindDescriptorSets cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 [descSet] []
        bindViewport device descSet viewportBuff
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
  Vk.DeviceSize ->
  Int ->
  Managed (V.Vector Frame)
withFrames device gfx allocator stagingBufferSize vertexBufferSize lightBufferSize frameCount = V.replicateM frameCount singleFrame
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
        viewportBuffer <- withGPUBuffer allocator lightBufferSize Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
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
              viewport = viewportBuffer,
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
worldEvent e w@(World {grid, gridStuff, pointer, cardSize, camera, pressedKeys}) =
  return
    w
      { grid = grid',
        pointer = pointer',
        pressedKeys = pressedKeys',
        camera = camera'
      }
  where
    grid' = if mouseClicked then flip else grid
    camera'
      | keyReleasedIs SDL.Scancode0 = camera {zoom = 1.0, rotation = 0, position = vec 0 0}
      | otherwise = camera
    pressedKeys'
      | Just code <- keyPressed = Set.insert code pressedKeys
      | Just code <- keyReleased = Set.delete code pressedKeys
      | otherwise = pressedKeys
    pointer' = fromMaybe w.pointer mouseMoved
    flip
      | Just spot <- spot = gridFlip spot grid
      | otherwise = grid
    spot =
      let WithVec x y = -gridStuff.topLeft + tr (pixelToWorld windowSize ppu camera) pointer
          WithVec px py = gridStuff.padding
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
gridFlip spot (Grid m) = Grid $ Map.adjust cardFlip spot m

cardFlip :: Card -> Card
cardFlip (Card name FaceUp) = Card name FaceDown
cardFlip (Card name FaceDown) = Card name FaceUp

worldTime :: (Monad io) => TimeSeconds -> World -> io World
worldTime (TimeSeconds dt) w = return w {camera = camera'}
  where
    camera' = foldl (flip ($)) w.camera cameraActions
    cameraActions =
      [moveCamera (vec 0 (moveSpeed * dt)) | Set.member SDL.ScancodeUp w.pressedKeys]
        ++ [moveCamera (vec 0 (-(moveSpeed * dt))) | Set.member SDL.ScancodeDown w.pressedKeys]
        ++ [moveCamera (vec (-(moveSpeed * dt)) 0) | Set.member SDL.ScancodeLeft w.pressedKeys]
        ++ [moveCamera (vec (moveSpeed * dt) 0) | Set.member SDL.ScancodeRight w.pressedKeys]
        ++ [rotateCamera (rotationSpeed * dt) | Set.member SDL.ScancodeE w.pressedKeys]
        ++ [rotateCamera (-(rotationSpeed * dt)) | Set.member SDL.ScancodeR w.pressedKeys]
        ++ [zoomInCamera (zoomStep * dt) | Set.member SDL.ScancodeEquals w.pressedKeys]
        ++ [zoomInCamera (-(zoomStep * dt)) | Set.member SDL.ScancodeMinus w.pressedKeys, w.camera.zoom >= minZoom]
    moveSpeed = 5 -- World units / s
    zoomStep = 1 -- 1 / s
    minZoom = 0.30
    rotationSpeed = (2 * pi) / 5 -- 360 in 5 seconds

windowSize :: ViewportSize
windowSize = vec 600 400

ppu :: PPU
ppu = PPU 100

ppu_1 :: PPU -> Float
ppu_1 (PPU ppu) = 1 / ppu

data Camera = Camera {position :: WorldVec, rotation :: Float, zoom :: Float}

rotateCamera :: Float -> Camera -> Camera
rotateCamera r cam = cam {rotation = cam.rotation + r}

moveCamera :: WorldVec -> Camera -> Camera
moveCamera v cam = cam {position = cam.position + v}

zoomCameraTo :: Float -> Camera -> Camera
zoomCameraTo z cam = cam {zoom = z}

zoomInCamera :: Float -> Camera -> Camera
zoomInCamera s cam = zoomCameraTo (cam.zoom + s) cam

view :: Camera -> SRT
view Camera {position = WithVec x y, rotation, zoom} = SRT.inv $ srt (zoom, zoom) rotation (x, y)

projection :: ViewportSize -> PPU -> Float -> SRT
projection (WithVec w h) (PPU ppu) zoom = srt (s w, -(s h)) 0 (0, 0)
  where
    s x = (2 * ppu * zoom) / fromIntegral x

pixelToWorld :: ViewportSize -> PPU -> Camera -> SRT
pixelToWorld (WithVec w h) (PPU ppu) cam =
  let WithVec x y = cam.position
      w2 = (fromIntegral w / 2) / ppu
      h2 = (fromIntegral h / 2) / ppu
      dx = x - w2
      dy = y + h2
      r = cam.rotation
      sx = cam.zoom / ppu
      sy = cam.zoom / ppu
   in srt (sx, -sy) (-r) (dx, dy)

sprites :: World -> [Tex.SpriteInWorld]
sprites World {atlas, grid = (Grid grid), gridStuff, pointer, cardSize, camera} = grd
  where
    pointerPosWorld = tr (pixelToWorld windowSize ppu camera) pointer
    spot pos =
      let WithVec x y = pos - gridStuff.topLeft
          WithVec w h = cardSize
          WithVec px py = gridStuff.padding
          r = floor $ (y + 1) / (h + px)
          c = floor $ (x + 1) / (w + py)
       in if (0 <= r && r <= 5) && (0 <= c && c <= 5) then Just (Spot (Row r, Column c)) else Nothing -- TODO: hardcoded "5"
      -- grd = uncurry putAt <$> Map.toList grid
    grd =
      [ putAt (Spot (Row 0, Column 0)) (Card (CardName "1") FaceUp),
        putAt (Spot (Row 1, Column 0)) (Card (CardName "2") FaceUp),
        putAt (Spot (Row 0, Column 1)) (Card (CardName "3") FaceUp)
      ]
    highlight spot =
      let border = putInWorld (Atlas.sprite atlas "border") pointerPosWorld
       in -- tr = transform $ pos spot + (topLeft :: NDCVec)
          border
      where
        GridStuff {topLeft = WithVec top left, padding = WithVec hPadding vPadding} = gridStuff
        WithVec w h = cardSize
    putAt (Spot (Row r, Column c)) crd = let s = putInWorld (card crd) pos in s
      where
        pos = vec (fromIntegral c) (fromIntegral r)
        rot = fromIntegral c * (pi / 16.0)
        card (Card (CardName name) FaceUp) = Atlas.sprite atlas name
        card (Card _ FaceDown) = faceDown
        GridStuff {topLeft = WithVec top left, padding = WithVec hPadding vPadding} = gridStuff
        -- pos2 (Spot (Row r, Column c)) = vec (fromIntegral c * (w + hPadding) + left) (fromIntegral r * (h + vPadding) + top)
        WithVec w h = cardSize
    faceDown = Atlas.sprite atlas "back-side"

screenSprites :: World -> [Tex.SpriteInScreen]
screenSprites (World {pointer, atlas}) =
  let -- ptr = (Atlas.sprite atlas "pointer") {Tex.origin = topLeft}
      -- p = Tex.putInScreen  topLeft (pixelPosToNdc windowSize pointer)
      r0 = (Atlas.spriteIndexed atlas "rectangle" 0) {Tex.origin = vec 0 0}
      r1 = (Atlas.spriteIndexed atlas "rectangle" 1) {Tex.origin = vec 100 0}
      r2 = (Atlas.spriteIndexed atlas "rectangle" 2) {Tex.origin = vec 100 50}
      r3 = (Atlas.spriteIndexed atlas "rectangle" 3) {Tex.origin = vec 0 50}
      r4 = (Atlas.spriteIndexed atlas "rectangle" 4) {Tex.origin = vec 50 25}
   in --  in [ rot (pi / 16) $ Tex.putInScreen r0 topLeft,
      --       scl (G.vec2 1.1 1.1) $ Tex.putInScreen r1 topRight,
      [ Tex.putInScreen r0 topLeft,
        Tex.putInScreen r1 topRight,
        Tex.putInScreen r2 bottomRight,
        Tex.putInScreen r3 bottomLeft,
        Tex.putInScreen r4 center
      ]
  where
    rot r s = s {Tex.rotation = r} :: Tex.SpriteInScreen
    scl k s = s {Tex.scale = k} :: Tex.SpriteInScreen

screenVertices :: ViewportSize -> Tex.SpriteInScreen -> SV.Vector Vert.Vertex
screenVertices
  ws
  (Tex.SpriteInScreen {sprite, position = WithVec x y, rotation, scale = G.WithVec2 sx sy}) =
    let WithVec w h = sprite.resolution
        top = 0
        left = 0
        right = w
        bottom = h
        UVReg2 auv buv cuv duv = sprite.region
        a = vert left top auv -- top left
        b = vert right top buv -- top right
        c = vert right bottom cuv -- bottom right
        d = vert left bottom duv -- bottom left
     in SV.fromList [a, b, c, c, d, a]
    where
      vert x y uv = Vert.Vertex {xy = tr2 @WorldVec (local <> embed) x y, uv = uv, texture = sprite.texture}
      embed = Tex.embedIntoScreen ws sprite.origin
      local = srt (sx, sy) rotation (x, y)

vertices :: Camera -> Tex.SpriteInWorld -> SV.Vector Vert.Vertex
vertices
  cam
  (Tex.SpriteInWorld {sprite, position = (WithVec x y), rotation, scale = (G.WithVec2 sx sy)}) =
    let WithVec w h = sprite.resolution
        top = 0
        left = 0
        right = w
        bottom = h
        UVReg2 auv buv cuv duv = sprite.region
        a = vert left top auv -- top left
        b = vert right top buv -- top right
        c = vert right bottom cuv -- bottom right
        d = vert left bottom duv -- bottom left
     in SV.fromList [a, b, c, c, d, a]
    where
      vert x y uv = Vert.Vertex {xy = tr2 @WorldVec transform x y, uv = uv, texture = sprite.texture}
      model = let s = ppu_1 ppu; WithVec x y = sprite.origin in srt (s, -s) rotation (x, y) -- Pixel (object space) to parent
      local = srt (sx, sy) rotation (x, y) -- Place in parent
      v = view cam
      p = projection windowSize ppu cam.zoom
      transform = p <> v <> local <> model

-- transform =  object <> local <> v <> p

descriptorSetLayout :: Vk.Device -> Word32 -> Managed Vk.DescriptorSetLayout
descriptorSetLayout dev count = do
  let flags = Vk.DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT .|. Vk.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT
      textures =
        Vk.zero
          { VkDescriptorSetLayoutBinding.binding = 0, -- TODO: hardcoded
            VkDescriptorSetLayoutBinding.descriptorCount = count,
            VkDescriptorSetLayoutBinding.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
            VkDescriptorSetLayoutBinding.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
          }
      lights =
        Vk.zero
          { VkDescriptorSetLayoutBinding.binding = 1, -- TODO: hardcoded
            VkDescriptorSetLayoutBinding.descriptorCount = 1,
            VkDescriptorSetLayoutBinding.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            VkDescriptorSetLayoutBinding.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
          }
      viewport =
        Vk.zero
          { VkDescriptorSetLayoutBinding.binding = 2, -- TODO: hardcoded
            VkDescriptorSetLayoutBinding.descriptorCount = 1,
            VkDescriptorSetLayoutBinding.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            VkDescriptorSetLayoutBinding.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
          }
      bindings = [textures, lights, viewport]
      flagsInfo =
        Vk.zero
          { VkDescriptorSetLayoutBindingFlagsCreateInfo.bindingFlags = flags <$ bindings
          }
      layoutInfo =
        Vk.zero
          { VkDescriptorSetLayoutCreateInfo.bindings = bindings,
            VkDescriptorSetLayoutCreateInfo.flags = Vk.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
          }
          ::& flagsInfo :& ()
  managed $ Vk.withDescriptorSetLayout dev layoutInfo Nothing bracket

descriptorPool :: Vk.Device -> Word32 -> Managed Vk.DescriptorPool
descriptorPool dev textureCount =
  let poolSize typ =
        Vk.zero
          { VkDescriptorPoolSize.descriptorCount = textureCount,
            VkDescriptorPoolSize.type' = typ
          }
      types =
        [ Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
          Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
        ]
      info =
        Vk.zero
          { VkDescriptorPoolCreateInfo.poolSizes = poolSize <$> types,
            VkDescriptorPoolCreateInfo.maxSets = 1,
            VkDescriptorPoolCreateInfo.flags =
              Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT .|. Vk.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT
          }
   in managed $ Vk.withDescriptorPool dev info Nothing bracket

descriptorSet :: Vk.Device -> Vk.DescriptorSetLayout -> Vk.DescriptorPool -> Managed Vk.DescriptorSet
descriptorSet dev layout pool =
  let info =
        Vk.zero
          { VkDescriptorSetAllocateInfo.descriptorPool = pool,
            VkDescriptorSetAllocateInfo.setLayouts = [layout]
          }
   in V.head <$> managed (Vk.withDescriptorSets dev info bracket)

bindViewport :: (MonadIO io) => Vk.Device -> Vk.DescriptorSet -> Vk.Buffer -> io ()
bindViewport dev set viewport =
  let info =
        Vk.SomeStruct
          Vk.zero
            { VkWriteDescriptorSet.dstSet = set,
              VkWriteDescriptorSet.dstBinding = 2, -- TODO: magic number, use a configurable value
              VkWriteDescriptorSet.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
              VkWriteDescriptorSet.descriptorCount = 1,
              VkWriteDescriptorSet.bufferInfo = [bufferInfo],
              VkWriteDescriptorSet.dstArrayElement = 0
            }
   in Vk.updateDescriptorSets dev [info] []
  where
    bufferInfo =
      Vk.zero
        { VkDescriptorBufferInfo.buffer = viewport,
          VkDescriptorBufferInfo.offset = 0,
          VkDescriptorBufferInfo.range = Vk.WHOLE_SIZE -- TODO: better value ?
        }

createPipeline ::
  Vk.Device ->
  Vk.Extent2D ->
  Vk.PipelineLayout ->
  Managed Vk.Pipeline
createPipeline dev extent layout = do
  (vert, frag) <- Init.withShaders dev
  (_, res) <-
    let vertextAttribute format location offset =
          Vk.zero
            { VkVertexInputAttributeDescription.binding = 0,
              VkVertexInputAttributeDescription.location = location,
              VkVertexInputAttributeDescription.format = format,
              VkVertexInputAttributeDescription.offset = fromIntegral offset
            }
        posSize = sizeOf (undefined :: G.Vec2)
        texCordSize = sizeOf (undefined :: G.Vec2)
        attributes =
          [ vertextAttribute Vk.FORMAT_R32G32_SFLOAT 0 (0 :: Int), -- position
            vertextAttribute Vk.FORMAT_R32G32_SFLOAT 1 posSize, -- texture coordinates
            vertextAttribute Vk.FORMAT_R32_UINT 2 (posSize + texCordSize) -- texture index
          ]
        vertexInputInfo =
          Just $
            Vk.SomeStruct
              Vk.zero
                { VkPipelineVertexInputStateCreateInfo.vertexBindingDescriptions =
                    [ Vk.zero
                        { VkVertexInputBindingDescription.binding = 0,
                          VkVertexInputBindingDescription.stride = fromIntegral $ sizeOf (undefined :: Vert.Vertex),
                          VkVertexInputBindingDescription.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
                        }
                    ],
                  VkPipelineVertexInputStateCreateInfo.vertexAttributeDescriptions = attributes
                }
        dynamicRendering =
          Vk.zero
            { VkPipelineRenderingCreateInfo.colorAttachmentFormats = [Init.imageFormat]
            }
        inputAssembly =
          Just
            Vk.zero
              { Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
                Vk.primitiveRestartEnable = False
              }
        viewport =
          Just . Vk.SomeStruct $
            Vk.zero
              { Vk.viewports =
                  [ Vk.Viewport
                      { VkViewport.x = 0,
                        VkViewport.y = 0,
                        VkViewport.width = realToFrac (VkExtent2D.width extent),
                        VkViewport.height = realToFrac (VkExtent2D.height extent),
                        VkViewport.minDepth = 0,
                        VkViewport.maxDepth = 1
                      }
                  ],
                Vk.scissors =
                  [Vk.Rect2D {VkRect2D.offset = Vk.Offset2D 0 0, VkRect2D.extent = extent}]
              }
        rasterization =
          Just . Vk.SomeStruct $
            Vk.zero
              { Vk.depthClampEnable = False,
                Vk.rasterizerDiscardEnable = False,
                Vk.lineWidth = 1,
                Vk.polygonMode = Vk.POLYGON_MODE_FILL,
                Vk.cullMode = Vk.CULL_MODE_NONE,
                Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE,
                Vk.depthBiasEnable = False
              }
        multisample =
          Just . Vk.SomeStruct $
            Vk.zero
              { VkVPipelineMultisampleStateCreateInfo.sampleShadingEnable = False,
                VkVPipelineMultisampleStateCreateInfo.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT,
                VkVPipelineMultisampleStateCreateInfo.minSampleShading = 1,
                VkVPipelineMultisampleStateCreateInfo.sampleMask = [maxBound]
              }
        colorBlend =
          Just . Vk.SomeStruct $
            Vk.zero
              { VkPipelineColorBlendStateCreateInfo.logicOpEnable = False,
                VkPipelineColorBlendStateCreateInfo.attachments =
                  [ Vk.zero
                      { Vk.colorWriteMask =
                          Vk.COLOR_COMPONENT_R_BIT
                            .|. Vk.COLOR_COMPONENT_G_BIT
                            .|. Vk.COLOR_COMPONENT_B_BIT
                            .|. Vk.COLOR_COMPONENT_A_BIT,
                        Vk.srcColorBlendFactor = Vk.BLEND_FACTOR_SRC_ALPHA,
                        Vk.dstColorBlendFactor = Vk.BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                        Vk.colorBlendOp = Vk.BLEND_OP_ADD,
                        Vk.srcAlphaBlendFactor = Vk.BLEND_FACTOR_ONE,
                        Vk.dstAlphaBlendFactor = Vk.BLEND_FACTOR_ZERO,
                        Vk.alphaBlendOp = Vk.BLEND_OP_ADD,
                        Vk.blendEnable = True
                      }
                  ]
              }
        pipelineCreateInfo =
          Vk.zero
            { VkGraphicsPipelineCreateInfo.stages = [vert, frag],
              VkGraphicsPipelineCreateInfo.vertexInputState = vertexInputInfo,
              VkGraphicsPipelineCreateInfo.inputAssemblyState = inputAssembly,
              VkGraphicsPipelineCreateInfo.viewportState = viewport,
              VkGraphicsPipelineCreateInfo.rasterizationState = rasterization,
              VkGraphicsPipelineCreateInfo.multisampleState = multisample,
              VkGraphicsPipelineCreateInfo.depthStencilState = Nothing,
              VkGraphicsPipelineCreateInfo.colorBlendState = colorBlend,
              VkGraphicsPipelineCreateInfo.dynamicState = Nothing,
              VkGraphicsPipelineCreateInfo.layout = layout,
              VkGraphicsPipelineCreateInfo.subpass = 0,
              VkGraphicsPipelineCreateInfo.basePipelineHandle = Vk.zero
            }
            ::& dynamicRendering :& ()
     in managed $ Vk.withGraphicsPipelines dev Vk.zero [Vk.SomeStruct pipelineCreateInfo] Nothing bracket
  return $ V.head res

clearColor :: Vk.ClearValue
clearColor = Vk.Color (Vk.Float32 1.0 0.0 1.0 0)
