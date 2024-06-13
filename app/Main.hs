{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Codec.Picture qualified as JP
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Managed (Managed, MonadIO (liftIO), managed, runManaged)
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as BS (readFile)
import Data.Foldable (foldlM)
import Data.Functor (($>), (<&>))
import Data.Vector ((!))
import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import Foreign (Bits (zeroBits), Ptr, Storable, Word32, castPtr, copyArray, withForeignPtr)
import Foreign.Ptr (castFunPtr)
import Foreign.Storable (Storable (..), sizeOf)
import Foreign.Storable.Record qualified as Store
import Geomancy qualified as G
import SDL qualified
import Utils
import Vulkan qualified as Vk
import Vulkan qualified as VkBufferCreateInfo (BufferCreateInfo (..))
import Vulkan qualified as VkCommandPoolCreateInfo (CommandPoolCreateInfo (..))
import Vulkan qualified as VkDescriptorImageInfo (DescriptorImageInfo (..))
import Vulkan qualified as VkDescriptorPoolCreateInfo (DescriptorPoolCreateInfo (..))
import Vulkan qualified as VkDescriptorPoolSize (DescriptorPoolSize (..))
import Vulkan qualified as VkDescriptorSetAllocateInfo (DescriptorSetAllocateInfo (..))
import Vulkan qualified as VkDescriptorSetLayoutBinding (DescriptorSetLayoutBinding (..))
import Vulkan qualified as VkDescriptorSetLayoutBindingFlagsCreateInfo (DescriptorSetLayoutBindingFlagsCreateInfo (..))
import Vulkan qualified as VkDescriptorSetLayoutCreateInfo (DescriptorSetLayoutCreateInfo (..))
import Vulkan qualified as VkDevice (Device (..))
import Vulkan qualified as VkDeviceCreateInfo (DeviceCreateInfo (..))
import Vulkan qualified as VkDeviceQueueCreateInfo (DeviceQueueCreateInfo (..))
import Vulkan qualified as VkExtent2D (Extent2D (..))
import Vulkan qualified as VkExtent3D (Extent3D (..))
import Vulkan qualified as VkFenceCreateInfo (FenceCreateInfo (..))
import Vulkan qualified as VkGraphicsPipelineCreateInfo (GraphicsPipelineCreateInfo (..))
import Vulkan qualified as VkImageCreateInfo (ImageCreateInfo (..))
import Vulkan qualified as VkImageSubresourceRange (ImageSubresourceRange (..))
import Vulkan qualified as VkImageViewCreateInfo (ImageViewCreateInfo (..))
import Vulkan qualified as VkInstance (Instance (..))
import Vulkan qualified as VkPhysicalDeviceDynamicRenderingFeatures (PhysicalDeviceDynamicRenderingFeatures (..))
import Vulkan qualified as VkPhysicalDeviceFeatures2 (PhysicalDeviceFeatures2 (..))
import Vulkan qualified as VkPhysicalDeviceVulkan12Features (PhysicalDeviceVulkan12Features (..))
import Vulkan qualified as VkPipelineColorBlendStateCreateInfo (PipelineColorBlendStateCreateInfo (..))
import Vulkan qualified as VkPipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import Vulkan qualified as VkPipelineRenderingCreateInfo (PipelineRenderingCreateInfo (..))
import Vulkan qualified as VkPipelineShaderStageCreateInfo (PipelineShaderStageCreateInfo (..))
import Vulkan qualified as VkPipelineVertexInputStateCreateInfo (PipelineVertexInputStateCreateInfo (..))
import Vulkan qualified as VkPresentInfoKHR (PresentInfoKHR (..))
import Vulkan qualified as VkRect2D (Rect2D (..))
import Vulkan qualified as VkRenderingAttachmentInfo (RenderingAttachmentInfo (..))
import Vulkan qualified as VkRenderingInfo (RenderingInfo (..))
import Vulkan qualified as VkSamplerCreateInfo (SamplerCreateInfo (..))
import Vulkan qualified as VkShaderModuleCreateInfo (ShaderModuleCreateInfo (..))
import Vulkan qualified as VkSubmitInfo (SubmitInfo (..))
import Vulkan qualified as VkSurface (SurfaceFormatKHR (..))
import Vulkan qualified as VkSurfaceCaps (SurfaceCapabilitiesKHR (..))
import Vulkan qualified as VkSwapchainCreateInfo (SwapchainCreateInfoKHR (..))
import Vulkan qualified as VkVPipelineMultisampleStateCreateInfo (PipelineMultisampleStateCreateInfo (..))
import Vulkan qualified as VkVertexInputAttributeDescription (VertexInputAttributeDescription (..))
import Vulkan qualified as VkVertexInputBindingDescription (VertexInputBindingDescription (..))
import Vulkan qualified as VkViewport (Viewport (..))
import Vulkan qualified as VkWriteDescriptorSet (WriteDescriptorSet (..))
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import Vulkan.CStruct.Extends qualified as Vk
import Vulkan.Dynamic qualified as Vk
import Vulkan.Zero qualified as Vk
import VulkanMemoryAllocator qualified as Vma
import VulkanMemoryAllocator qualified as VmaAllocationCreateInfo (AllocationCreateInfo (..))
import VulkanMemoryAllocator qualified as VmaAllocatorCreateInfo (AllocatorCreateInfo (..))
import Prelude hiding (init)

data Vertex = Vertex
  { xy :: G.Vec2,
    rgb :: G.Vec3,
    uv :: G.Vec2,
    texture :: Word32
  }
  deriving (Show, Eq)

store :: Store.Dictionary Vertex
store =
  Store.run $
    Vertex
      <$> Store.element (\Vertex {xy} -> xy)
      <*> Store.element (\Vertex {rgb} -> rgb)
      <*> Store.element (\Vertex {uv} -> uv)
      <*> Store.element (\Vertex {texture} -> texture)

instance Storable Vertex where
  sizeOf = Store.sizeOf store
  alignment = Store.alignment store
  peek = Store.peek store
  poke = Store.poke store

data Sprite = Sprite
  { pos :: G.Vec2,
    scale :: G.Vec2,
    texture :: Texture,
    frame :: G.Vec4
  }

data Sheet = Sheet {texture :: Texture, frames :: V.Vector G.Vec4}

sheetFrame :: Sheet -> Int -> G.Vec4
sheetFrame Sheet {frames} n = frames ! n

sheetFrameCount :: Sheet -> Int
sheetFrameCount Sheet {frames} = V.length frames

sheetClipFrameIndex :: Sheet -> Float -> Float
sheetClipFrameIndex sheet f = if f >= fromIntegral (sheetFrameCount sheet) then 0.0 else f

animatedProgress :: Word32 -> Animated -> Animated
animatedProgress dt ani@Animated {animation = Animation {sheet, speed = FramesPerSecond speed}, frameIndex} =
  let f = frameIndex + (speed * fromIntegral dt) / 1000
   in ani {frameIndex = sheetClipFrameIndex sheet f}

animaredFrame :: Animated -> G.Vec4
animaredFrame Animated {animation = Animation {sheet}, frameIndex} = sheetFrame sheet (floor frameIndex)

newtype FramesPerSecond = FramesPerSecond Float

data Animation = Animation {sheet :: Sheet, speed :: FramesPerSecond}

data Animated = Animated {animation :: Animation, frameIndex :: Float}

data Object = Object {sprite :: Sprite, vel :: G.Vec2, animation :: Maybe Animated}

data World = World
  { background :: Sprite,
    pointer :: Sprite,
    a :: Object,
    b :: Object,
    c :: Object
  }

data LoadedTexture = LoadedTexture {resolution :: G.UVec2, size :: G.Vec2, image :: Vk.Image, view :: Vk.ImageView}

data Texture = Texture {index :: Word32, texture :: LoadedTexture}

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

main :: IO ()
main = runManaged $ do
  withSDL
  window <- withWindow windowWidth windowHeight
  vulkan <- withVulkan window
  _ <- withDebug vulkan
  surface <- withSurface window vulkan
  (gpu, gfx, present, portable) <- pickGPU vulkan surface >>= maybe (sayErr "Vulkan" "Suitable GPU not found") return
  props <- Vk.getPhysicalDeviceProperties gpu
  say "Vulkan" $ "GPU " ++ show (Vk.deviceName props) ++ ", present queue " ++ show present ++ ", graphics queue " ++ show gfx
  say "Vulkan" "Creating device"
  device <- withDevice gpu present gfx portable <* say "Vulkan" "Device created"
  say "Vulkan" "Creating swap chain"
  commandPool <-
    let info =
          Vk.zero
            { VkCommandPoolCreateInfo.queueFamilyIndex = gfx,
              VkCommandPoolCreateInfo.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
            }
     in managed $ Vk.withCommandPool device info Nothing bracket
  say "Vulkan" "Created command pool"
  allocator <- withMemoryAllocator vulkan gpu device <* say "VMA" "Created allocator"
  say "Vulkan" "Created staging vertex buffer"
  descSetLayout <- descriptorSetLayout device 1000
  descPool <- descriptorPool device 1000
  descSet <- descriptorSet device descSetLayout descPool
  sampler <- repeatingSampler device
  gfxQueue <- Vk.getDeviceQueue device gfx 0 <* say "Vulkan" "Got graphics queue"
  textures <-
    let textures = ["checkerboard", "crosshair", "basketball", "animation", "blue_ball"] :: [String]
     in traverse (\tex -> let file = "textures/" ++ tex ++ ".png" in readTexture allocator device commandPool gfxQueue file) textures
  [background, pointer, texture1, texture2, texture3] <- bindTextures device descSet textures sampler
  SDL.showWindow window <* say "SDL" "Show window"
  SDL.raiseWindow window <* say "SDL" "Raise window"
  (SDL.cursorVisible SDL.$= False) <* say "SDL" "Dsiable cursor"
  -- withImGui vulkan gpu device window gfxQueue commandPool gfxQueue
  let p0 = G.vec2 (-0.5) (-0.5)
      one = G.vec2 1 1
      half = G.vec2 0.5 0.5
      third = G.vec2 0.33 0.33
      viewFull = G.vec4 0 0 1 1
      viewFrame = G.vec4 0.0 0.0 0.143 0.25
      sheet =
        Sheet
          { texture = texture2,
            frames =
              V.fromList
                [ let width = 0.143
                      height = 0.25
                      row = frameNumber `mod` 7
                      col = frameNumber `mod` 4
                      x = fromIntegral col * 0.143
                      y = fromIntegral row * 0.25
                   in G.vec4 x y (x + width) (y + height)
                  | frameNumber <- [0 .. 26 :: Int]
                ]
          }
      b = Sprite {pos = p0, scale = one, texture = texture2, frame = sheetFrame sheet 0}
      animation = Animation {sheet = sheet, speed = FramesPerSecond 15.0}
      world0 =
        World
          { background = Sprite {pos = G.vec2 (-1.0) (-1.0), scale = G.vec2 0.2 0.2, texture = background, frame = viewFull},
            pointer = Sprite {pos = G.vec2 0.0 0.0, texture = pointer, scale = half, frame = viewFull},
            a = Object {sprite = Sprite {pos = p0, scale = one, texture = texture1, frame = viewFull}, vel = G.vec2 0.002 0.001, animation = Nothing},
            b = Object {sprite = b, vel = G.vec2 0.0 0.0, animation = Just $ Animated animation 0.0},
            c = Object {sprite = Sprite {pos = p0, scale = one, texture = texture3, frame = viewFull}, vel = G.vec2 0.001 0.002, animation = Nothing}
          }

  swapchain@(_, swapchainExtent, swapchainImages) <-
    createSwapchain
      gpu
      device
      surface
      gfx
      present
      windowWidth
      windowHeight
  let frameCount = V.length swapchainImages
  say "Engin" $ "Frame count is " ++ show frameCount

  pipelineLayout <-
    let info = Vk.zero {VkPipelineLayoutCreateInfo.setLayouts = [descSetLayout]}
     in managed $ Vk.withPipelineLayout device info Nothing bracket
  pipeline <- createPipeline device swapchainExtent pipelineLayout
  presentQueue <- Vk.getDeviceQueue device present 0 <* say "Vulkan" "Got present queue"

  let stagingBufferSize = 1048576
      vertexBufferSize = 4 * 1024
  frames <- withFrames device gfx allocator stagingBufferSize vertexBufferSize frameCount
  let shutdown =
        do
          say "Engine" "Shutting down ..."
          Vk.deviceWaitIdle device
   in say "Engine" "Entering the main loop"
        *> mainLoop
          shutdown
          (vertices . sprites)
          ( \frameNumber verts ->
              do
                let n = frameNumber `mod` frameCount
                    f = frames ! n
                 in frame device gfxQueue presentQueue pipeline pipelineLayout swapchain descSet f verts
          )
          worldTime
          worldEvent
          world0

frame ::
  (Storable a, MonadIO m) =>
  Vk.Device ->
  Vk.Queue ->
  Vk.Queue ->
  Vk.Pipeline ->
  Vk.PipelineLayout ->
  (Vk.SwapchainKHR, VkExtent2D.Extent2D, V.Vector Vk.Image) ->
  Vk.DescriptorSet ->
  Frame ->
  SV.Vector a ->
  m ()
frame device gfxQueue presentQueue pipeline pipelineLayout swp descSet f verts = do
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
      (swapchain, swapchainExtent, swapchainImages) = swp
  waitForFrame device f
  liftIO $ copyToGpu device pool gfxQueue vertex staging verts

  recordRender renderCmd vertex (targetImage, targetView) swapchainExtent
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
   in Vk.queueSubmit gfxQueue [render, copy] fence
  let info =
        Vk.zero
          { VkPresentInfoKHR.waitSemaphores = [copyFinished],
            VkPresentInfoKHR.swapchains = [swapchain],
            VkPresentInfoKHR.imageIndices = [index]
          }
   in Vk.queuePresentKHR presentQueue info >>= checkSwapchainIsOld
  where
    checkSwapchainIsOld r = when (r == Vk.SUBOPTIMAL_KHR || r == Vk.ERROR_OUT_OF_DATE_KHR) (say "Engine" $ "presentFrame" ++ show r)
    recordCopyToSwapchain cmd offscreenImage swapchainImage extent =
      Vk.useCommandBuffer cmd Vk.zero $ do
        transitRenderTargetToCopySrc cmd offscreenImage
        transitToCopyDst cmd swapchainImage
        copyImageToImage cmd offscreenImage swapchainImage extent
        transitToPresent cmd swapchainImage
    recordRender cmd vertBuff target extent =
      Vk.useCommandBuffer cmd Vk.zero $ do
        let (image, view) = target
        Vk.cmdBindPipeline cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
        Vk.cmdBindVertexBuffers cmd 0 [vertBuff] [0]
        Vk.cmdBindDescriptorSets cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 [descSet] []
        transitToRenderTarget cmd image
        let vertexCount = fromIntegral $ SV.length verts
            attachment =
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
        vertextStagingBuffer <- withHostBuffer allocator stagingBufferSize
        imageAvailable <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
        renderFinished <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
        copyFinished <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
        fence <-
          let info = Vk.zero {VkFenceCreateInfo.flags = Vk.FENCE_CREATE_SIGNALED_BIT}
           in managed $ Vk.withFence device info Nothing bracket
        vertexBuffer <- withGPUBuffer allocator vertexBufferSize Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
        (image, view) <- withImageAndView allocator device windowWidth windowHeight imageFormat
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
  (w -> SV.Vector Vertex) ->
  (Int -> SV.Vector Vertex -> io ()) ->
  (Word32 -> w -> io w) ->
  (SDL.Event -> w -> io w) ->
  w ->
  io ()
mainLoop shutdown vertices frame worldTime worldEvent world0 =
  do
    t0 <- SDL.ticks
    go 0 t0 world0
  where
    lockFrameRate fps t1 =
      do
        t <- SDL.ticks
        let dt = t - t1
            minIdle = 1000 `div` fps
        if dt < minIdle then (liftIO . threadDelay $ 1000 * fromIntegral (minIdle - dt)) *> SDL.ticks else pure t
    go frameNumber t w = do
      -- es <- ImGui.pollEventsWithImGui
      es <- SDL.pollEvents
      if any isQuitEvent es
        then shutdown
        else do
          frame frameNumber $ vertices w
          w2 <- foldlM (flip worldEvent) w es
          t2 <- lockFrameRate 60 t
          w3 <- worldTime (t2 - t) w2
          go (frameNumber + 1) t2 w3

worldEvent :: (Monad io) => SDL.Event -> World -> io World
worldEvent e w@(World {pointer = pointer@Sprite {pos = pos}}) = return w {pointer = pointer {pos = f pos e}}
  where
    f _ (SDL.Event _ (SDL.MouseMotionEvent (SDL.MouseMotionEventData {mouseMotionEventPos = SDL.P (SDL.V2 x y)}))) = normalPos x y
    f p _ = p

worldTime :: (Monad io) => Word32 -> World -> io World
worldTime 0 w = return w
worldTime dt w@(World {a, b, c}) = return w {a = update a, b = update b, c = update c}
  where
    update obj@Object {sprite = sprite@Sprite {pos = p0}, vel = v0, animation = Nothing} =
      let p1 = p0 + (v0 G.^* fromIntegral dt)
          v1 = G.emap2 (\vi pos -> if pos >= 1.0 || pos <= -1.0 then -vi else vi) v0 p1
       in obj {sprite = sprite {pos = p1}, vel = v1}
    update obj@Object {sprite, animation = Just ani} =
      let ani2 = animatedProgress dt ani
       in obj {sprite = sprite {frame = animaredFrame ani2}, animation = Just ani2}

repeatingSampler :: Vk.Device -> Managed Vk.Sampler
repeatingSampler device =
  let info =
        Vk.zero
          { VkSamplerCreateInfo.magFilter = Vk.FILTER_LINEAR,
            VkSamplerCreateInfo.minFilter = Vk.FILTER_LINEAR,
            VkSamplerCreateInfo.addressModeU = Vk.SAMPLER_ADDRESS_MODE_REPEAT,
            VkSamplerCreateInfo.addressModeV = Vk.SAMPLER_ADDRESS_MODE_REPEAT,
            VkSamplerCreateInfo.addressModeW = Vk.SAMPLER_ADDRESS_MODE_REPEAT,
            VkSamplerCreateInfo.unnormalizedCoordinates = False,
            VkSamplerCreateInfo.borderColor = Vk.BORDER_COLOR_INT_OPAQUE_WHITE
          }
   in managed $ Vk.withSampler device info Nothing bracket

windowWidth :: Int
windowWidth = 500

windowHeight :: Int
windowHeight = 500

normalSize :: G.UVec2 -> G.Vec2
normalSize (G.WithUVec2 w h) =
  let x' = (2.0 * fromIntegral w / fromIntegral windowWidth)
      y' = (2.0 * fromIntegral h / fromIntegral windowHeight)
   in G.vec2 x' y'

normalPos :: (Integral i) => i -> i -> G.Vec2
normalPos x y =
  let x' = (2.0 * fromIntegral x / fromIntegral windowWidth) - 1.0
      y' = (2.0 * fromIntegral y / fromIntegral windowHeight) - 1.0
   in G.vec2 x' y'

sprites :: World -> [Sprite]
sprites (World background pointer a b c) =
  [ background,
    s a,
    s b,
    s c,
    pointer
  ]
  where
    s Object {sprite} = sprite

withMemoryAllocator :: Vk.Instance -> Vk.PhysicalDevice -> Vk.Device -> Managed Vma.Allocator
withMemoryAllocator vulkan gpu device =
  let insanceCmds = VkInstance.instanceCmds vulkan
      deviceCmds = VkDevice.deviceCmds device
      info =
        Vk.zero
          { VmaAllocatorCreateInfo.vulkanApiVersion = vulkanVersion,
            VmaAllocatorCreateInfo.instance' = Vk.instanceHandle vulkan,
            VmaAllocatorCreateInfo.physicalDevice = Vk.physicalDeviceHandle gpu,
            VmaAllocatorCreateInfo.device = Vk.deviceHandle device,
            VmaAllocatorCreateInfo.vulkanFunctions =
              Just $
                Vk.zero
                  { Vma.vkGetInstanceProcAddr = castFunPtr $ Vk.pVkGetInstanceProcAddr insanceCmds,
                    Vma.vkGetDeviceProcAddr = castFunPtr $ Vk.pVkGetDeviceProcAddr deviceCmds
                  }
          }
   in managed $ Vma.withAllocator info bracket

vertices :: [Sprite] -> SV.Vector Vertex
vertices ss =
  let rgb = G.vec3 1.0 0.0 0.0
      toQuad
        Sprite
          { pos = G.WithVec2 x y,
            scale = G.WithVec2 sx sy,
            texture = Texture {index = tex, texture = LoadedTexture {size = G.WithVec2 tw th}},
            frame = (G.WithVec4 vx1 vy1 vx2 vy2)
          } =
          let vw = vx2 - vx1
              vh = vy2 - vy1
              w = tw * vw * sx
              h = th * vh * sy
              vuTopLeft = G.vec2 vx1 vy1
              vuTopRight = G.vec2 vx2 vy1
              vuBottomRight = G.vec2 vx2 vy2
              vuBottomLeft = G.vec2 vx1 vy2
              topLeft = Vertex {xy = G.vec2 x y, rgb = rgb, uv = vuTopLeft, texture = tex}
              topRight = Vertex {xy = G.vec2 (x + w) y, rgb = rgb, uv = vuTopRight, texture = tex}
              bottomRight = Vertex {xy = G.vec2 (x + w) (y + h), rgb = rgb, uv = vuBottomRight, texture = tex}
              bottomLeft = Vertex {xy = G.vec2 x (y + h), rgb = rgb, uv = vuBottomLeft, texture = tex}
           in SV.fromList
                [topLeft, topRight, bottomRight, bottomRight, bottomLeft, topLeft]
   in mconcat $ toQuad <$> ss

withImage :: Vma.Allocator -> Int -> Int -> Vk.Format -> Managed Vk.Image
withImage allocator width height format = do
  (image, _, _) <-
    let dims =
          Vk.Extent3D
            { VkExtent3D.width = fromIntegral width,
              VkExtent3D.height = fromIntegral height,
              VkExtent3D.depth = 1
            }
        usage = Vk.IMAGE_USAGE_TRANSFER_SRC_BIT .|. Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT .|. Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT -- Put only what we actually need
        imgInfo =
          Vk.zero
            { VkImageCreateInfo.imageType = Vk.IMAGE_TYPE_2D,
              VkImageCreateInfo.extent = dims,
              VkImageCreateInfo.mipLevels = 1,
              VkImageCreateInfo.arrayLayers = 1,
              VkImageCreateInfo.format = format,
              VkImageCreateInfo.tiling = Vk.IMAGE_TILING_OPTIMAL,
              VkImageCreateInfo.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
              VkImageCreateInfo.usage = usage,
              VkImageCreateInfo.samples = Vk.SAMPLE_COUNT_1_BIT
            }
        allocInfo =
          Vk.zero
            { VmaAllocationCreateInfo.usage = Vma.MEMORY_USAGE_AUTO,
              VmaAllocationCreateInfo.flags = Vma.ALLOCATION_CREATE_DEDICATED_MEMORY_BIT,
              VmaAllocationCreateInfo.priority = 1
            }
     in managed $ Vma.withImage allocator imgInfo allocInfo bracket
  return image

withImageAndView :: Vma.Allocator -> Vk.Device -> Int -> Int -> Vk.Format -> Managed (Vk.Image, Vk.ImageView)
withImageAndView allocator device width height format = do
  image <- withImage allocator width height format
  view <- withImageView device image format
  return (image, view)

readTexture :: Vma.Allocator -> Vk.Device -> Vk.CommandPool -> Vk.Queue -> FilePath -> Managed LoadedTexture
readTexture allocator device pool queue path = do
  JP.ImageRGBA8 (JP.Image width height pixels) <- liftIO $ JP.readPng path >>= either (sayErr "Texture" . show) return
  let size = width * height * 4
      format = Vk.FORMAT_R8G8B8A8_SRGB
  (image, view) <- withImageAndView allocator device width height format
  do
    (staging, mem) <- withHostBuffer allocator (fromIntegral size)
    liftIO $ copy pixels mem size
    copyBufferToImage device pool queue staging image width height
    let res = G.uvec2 (fromIntegral width) (fromIntegral height)
    return LoadedTexture {resolution = res, size = normalSize res, image = image, view = view}
  where
    copy pixels mem size =
      let (src, _) = SV.unsafeToForeignPtr0 pixels
          dst = castPtr mem
       in withForeignPtr src $ \from -> copyArray dst from size

withHostBuffer :: Vma.Allocator -> Vk.DeviceSize -> Managed (Vk.Buffer, Ptr ())
withHostBuffer allocator size = do
  (buffer, _, Vma.AllocationInfo {Vma.mappedData = mem}) <-
    let bufferInfo =
          Vk.zero
            { VkBufferCreateInfo.size = size,
              VkBufferCreateInfo.usage = Vk.BUFFER_USAGE_TRANSFER_SRC_BIT,
              VkBufferCreateInfo.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
            }
        vmaInfo =
          Vk.zero
            { VmaAllocationCreateInfo.usage = Vma.MEMORY_USAGE_AUTO,
              VmaAllocationCreateInfo.flags = Vma.ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT .|. Vma.ALLOCATION_CREATE_MAPPED_BIT
            }
     in managed $ Vma.withBuffer allocator bufferInfo vmaInfo bracket
  say "Vulkan" $ "Created host buffer (" ++ show size ++ " bytes)"
  return (buffer, mem)

withGPUBuffer :: Vma.Allocator -> Vk.DeviceSize -> Vk.BufferUsageFlagBits -> Managed Vk.Buffer
withGPUBuffer allocator size flags = do
  (buffer, _, _) <-
    let bufferInfo =
          Vk.zero
            { VkBufferCreateInfo.size = size,
              VkBufferCreateInfo.usage = Vk.BUFFER_USAGE_TRANSFER_DST_BIT .|. flags,
              VkBufferCreateInfo.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
            }
        vmaInfo =
          Vk.zero
            { VmaAllocationCreateInfo.usage = Vma.MEMORY_USAGE_AUTO,
              VmaAllocationCreateInfo.flags = Vma.ALLOCATION_CREATE_DEDICATED_MEMORY_BIT,
              VmaAllocationCreateInfo.priority = 1
            }
     in managed $ Vma.withBuffer allocator bufferInfo vmaInfo bracket
  say "Vulkan" $ "Created GPU buffer (" ++ show size ++ " bytes)"
  return buffer

descriptorSetLayout :: Vk.Device -> Word32 -> Managed Vk.DescriptorSetLayout
descriptorSetLayout dev count = do
  let flags = Vk.DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT .|. Vk.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT
      binding =
        Vk.zero
          { VkDescriptorSetLayoutBinding.binding = 0,
            VkDescriptorSetLayoutBinding.descriptorCount = count,
            VkDescriptorSetLayoutBinding.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
            VkDescriptorSetLayoutBinding.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
          }
      flagsInfo =
        Vk.zero
          { VkDescriptorSetLayoutBindingFlagsCreateInfo.bindingFlags = [flags]
          }
      layoutInfo =
        Vk.zero
          { VkDescriptorSetLayoutCreateInfo.bindings = [binding],
            VkDescriptorSetLayoutCreateInfo.flags = Vk.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
          }
          ::& flagsInfo :& ()
  managed $ Vk.withDescriptorSetLayout dev layoutInfo Nothing bracket

descriptorPool :: Vk.Device -> Word32 -> Managed Vk.DescriptorPool
descriptorPool dev textureCount = do
  let poolSize =
        Vk.zero
          { VkDescriptorPoolSize.descriptorCount = textureCount,
            VkDescriptorPoolSize.type' = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          }
      info =
        Vk.zero
          { VkDescriptorPoolCreateInfo.poolSizes = [poolSize],
            VkDescriptorPoolCreateInfo.maxSets = 1,
            VkDescriptorPoolCreateInfo.flags =
              Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT .|. Vk.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT
          }
   in managed $ Vk.withDescriptorPool dev info Nothing bracket

descriptorSet :: Vk.Device -> Vk.DescriptorSetLayout -> Vk.DescriptorPool -> Managed Vk.DescriptorSet
descriptorSet dev layout pool = do
  let info =
        Vk.zero
          { VkDescriptorSetAllocateInfo.descriptorPool = pool,
            VkDescriptorSetAllocateInfo.setLayouts = [layout]
          }
   in V.head <$> managed (Vk.withDescriptorSets dev info bracket)

bindTextures :: Vk.Device -> Vk.DescriptorSet -> [LoadedTexture] -> Vk.Sampler -> Managed [Texture]
bindTextures dev set textures sampler = do
  let info =
        Vk.SomeStruct
          Vk.zero
            { VkWriteDescriptorSet.dstSet = set,
              VkWriteDescriptorSet.dstBinding = 0,
              VkWriteDescriptorSet.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
              VkWriteDescriptorSet.descriptorCount = fromIntegral $ length textures,
              VkWriteDescriptorSet.imageInfo = V.fromList $ imageInfo <$> textures,
              VkWriteDescriptorSet.dstArrayElement = 0
            }
   in Vk.updateDescriptorSets dev [info] []
  return $ zipWith (\i t -> Texture {index = i, texture = t}) [0 ..] textures
  where
    imageInfo (LoadedTexture {view = v}) =
      Vk.zero
        { VkDescriptorImageInfo.imageView = v,
          VkDescriptorImageInfo.sampler = sampler,
          VkDescriptorImageInfo.imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        }

createPipeline ::
  Vk.Device ->
  Vk.Extent2D ->
  Vk.PipelineLayout ->
  Managed Vk.Pipeline
createPipeline dev extent layout = do
  (vert, frag) <- createShaders dev
  (_, res) <-
    let vertextAttribute format location offset =
          Vk.zero
            { VkVertexInputAttributeDescription.binding = 0,
              VkVertexInputAttributeDescription.location = location,
              VkVertexInputAttributeDescription.format = format,
              VkVertexInputAttributeDescription.offset = fromIntegral offset
            }
        posSize = sizeOf (undefined :: G.Vec2)
        colorSize = sizeOf (undefined :: G.Vec3)
        texCordSize = sizeOf (undefined :: G.Vec2)
        attributes =
          [ vertextAttribute Vk.FORMAT_R32G32_SFLOAT 0 (0 :: Int), -- position
            vertextAttribute Vk.FORMAT_R32G32B32_SFLOAT 1 (0 + posSize), -- color
            vertextAttribute Vk.FORMAT_R32G32_SFLOAT 2 (posSize + colorSize), -- texture coordinates
            vertextAttribute Vk.FORMAT_R32_UINT 3 (posSize + colorSize + texCordSize) -- texture index
          ]
        vertexInputInfo =
          Just $
            Vk.SomeStruct
              Vk.zero
                { VkPipelineVertexInputStateCreateInfo.vertexBindingDescriptions =
                    [ Vk.zero
                        { VkVertexInputBindingDescription.binding = 0,
                          VkVertexInputBindingDescription.stride = fromIntegral $ sizeOf (undefined :: Vertex),
                          VkVertexInputBindingDescription.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
                        }
                    ],
                  VkPipelineVertexInputStateCreateInfo.vertexAttributeDescriptions = attributes
                }
        dynamicRendering =
          Vk.zero
            { VkPipelineRenderingCreateInfo.colorAttachmentFormats = [imageFormat]
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

-- TODO break into two functions
createShaders ::
  Vk.Device ->
  Managed (Vk.SomeStruct Vk.PipelineShaderStageCreateInfo, Vk.SomeStruct Vk.PipelineShaderStageCreateInfo)
createShaders dev =
  do
    fragCode <- liftIO $ BS.readFile "frag.spv"
    vertCode <- liftIO $ BS.readFile "vert.spv"
    fragModule <- managed $ Vk.withShaderModule dev Vk.zero {VkShaderModuleCreateInfo.code = fragCode} Nothing bracket
    vertModule <- managed $ Vk.withShaderModule dev Vk.zero {VkShaderModuleCreateInfo.code = vertCode} Nothing bracket
    let vertextInfo =
          Vk.zero
            { VkPipelineShaderStageCreateInfo.stage = Vk.SHADER_STAGE_VERTEX_BIT,
              VkPipelineShaderStageCreateInfo.module' = vertModule,
              VkPipelineShaderStageCreateInfo.name = "main"
            }
        fragInfo =
          Vk.zero
            { VkPipelineShaderStageCreateInfo.stage = Vk.SHADER_STAGE_FRAGMENT_BIT,
              VkPipelineShaderStageCreateInfo.module' = fragModule,
              VkPipelineShaderStageCreateInfo.name = "main"
            }
    pure (Vk.SomeStruct vertextInfo, Vk.SomeStruct fragInfo)

presentMode :: Vk.PresentModeKHR
presentMode = Vk.PRESENT_MODE_FIFO_KHR

imageFormat :: Vk.Format
imageFormat = Vk.FORMAT_B8G8R8A8_SRGB

colorSpace :: Vk.ColorSpaceKHR
colorSpace = Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR

clearColor :: Vk.ClearValue
clearColor = Vk.Color (Vk.Float32 1.0 0.0 1.0 0)

createSwapchain ::
  Vk.PhysicalDevice ->
  Vk.Device ->
  Vk.SurfaceKHR ->
  Word32 ->
  Word32 ->
  Int ->
  Int ->
  Managed (Vk.SwapchainKHR, Vk.Extent2D, V.Vector Vk.Image)
createSwapchain
  gpu
  dev
  surface
  gfx
  present
  width
  height =
    do
      surcafeCaps <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR gpu surface
      let minImageCount = VkSurfaceCaps.minImageCount surcafeCaps
          transform = VkSurfaceCaps.currentTransform surcafeCaps
          (sharingMode, queues) =
            if gfx == present
              then (Vk.SHARING_MODE_EXCLUSIVE, [])
              else (Vk.SHARING_MODE_CONCURRENT, [gfx, present])
          extent = case VkSurfaceCaps.currentExtent surcafeCaps of
            Vk.Extent2D w h
              | w == maxBound,
                h == maxBound ->
                  Vk.Extent2D
                    (fromIntegral width)
                    (fromIntegral height)
            e -> e
          info =
            Vk.zero
              { VkSwapchainCreateInfo.surface = surface,
                VkSwapchainCreateInfo.minImageCount = minImageCount + 1,
                VkSwapchainCreateInfo.imageFormat = imageFormat,
                VkSwapchainCreateInfo.imageColorSpace = colorSpace,
                VkSwapchainCreateInfo.imageExtent = extent,
                VkSwapchainCreateInfo.imageArrayLayers = 1,
                VkSwapchainCreateInfo.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_TRANSFER_DST_BIT,
                VkSwapchainCreateInfo.imageSharingMode = sharingMode,
                VkSwapchainCreateInfo.queueFamilyIndices = queues,
                VkSwapchainCreateInfo.preTransform = transform,
                VkSwapchainCreateInfo.compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                VkSwapchainCreateInfo.presentMode = presentMode,
                VkSwapchainCreateInfo.clipped = True
              }
      swapchain <- managed $ Vk.withSwapchainKHR dev info Nothing bracket
      (_, images) <- Vk.getSwapchainImagesKHR dev swapchain
      return (swapchain, extent, images)

withImageView :: Vk.Device -> Vk.Image -> Vk.Format -> Managed Vk.ImageView
withImageView dev img format =
  let imageViewCreateInfo =
        Vk.zero
          { VkImageViewCreateInfo.image = img,
            VkImageViewCreateInfo.viewType = Vk.IMAGE_VIEW_TYPE_2D,
            VkImageViewCreateInfo.format = format,
            VkImageViewCreateInfo.subresourceRange =
              Vk.zero
                { VkImageSubresourceRange.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                  VkImageSubresourceRange.baseMipLevel = 0,
                  VkImageSubresourceRange.levelCount = 1,
                  VkImageSubresourceRange.baseArrayLayer = 0,
                  VkImageSubresourceRange.layerCount = 1
                }
          }
   in managed $ Vk.withImageView dev imageViewCreateInfo Nothing bracket

withDevice :: Vk.PhysicalDevice -> Word32 -> Word32 -> Bool -> Managed Vk.Device
withDevice gpu gfx present portable =
  let exts =
        Vk.KHR_DYNAMIC_RENDERING_EXTENSION_NAME
          : Vk.KHR_SWAPCHAIN_EXTENSION_NAME
          : Vk.KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME
          : ([Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME | portable])
      dynamicRendering = Vk.zero {VkPhysicalDeviceDynamicRenderingFeatures.dynamicRendering = True}
      bindlessDescriptors =
        Vk.zero
          { VkPhysicalDeviceVulkan12Features.runtimeDescriptorArray = True,
            VkPhysicalDeviceVulkan12Features.descriptorBindingPartiallyBound = True,
            VkPhysicalDeviceVulkan12Features.descriptorBindingSampledImageUpdateAfterBind = True
          }
      info =
        Vk.zero
          { VkDeviceCreateInfo.queueCreateInfos = V.fromList $ inf gfx : ([inf present | gfx /= present]),
            VkDeviceCreateInfo.enabledExtensionNames = V.fromList exts
          }
          ::& dynamicRendering
            :& bindlessDescriptors
            :& ()
   in managed $ Vk.withDevice gpu info Nothing bracket
  where
    inf i =
      Vk.SomeStruct $
        Vk.zero
          { VkDeviceQueueCreateInfo.queueFamilyIndex = i,
            VkDeviceQueueCreateInfo.queuePriorities = [1.0]
          }

pickGPU ::
  Vk.Instance ->
  Vk.SurfaceKHR ->
  Managed (Maybe (Vk.PhysicalDevice, Word32, Word32, Bool))
pickGPU vulkan surface = do
  (_, gpus) <- Vk.enumeratePhysicalDevices vulkan
  let good' d = ExceptT $ good d <&> (\case Nothing -> Right (); Just ((g, p), pss) -> Left (d, g, p, pss))
   in findM (\_ gpu -> good' gpu) () gpus
  where
    findM f s0 ta = runExceptT (foldlM f s0 ta) >>= (\r -> return $ case r of Left b -> Just b; Right _ -> Nothing)
    found = ExceptT . return . Left
    continue = ExceptT . return . Right

    support yes feature = liftIO (say "Enging" msg $> out)
      where
        msg = "GPU supports " ++ feature ++ ": " ++ show yes
        out = if yes then Just () else Nothing

    good gpu = do
      features <- Vk.getPhysicalDeviceFeatures2 gpu :: Managed (Vk.PhysicalDeviceFeatures2 '[Vk.PhysicalDeviceVulkan12Features])
      _ <-
        let f = fst $ VkPhysicalDeviceFeatures2.next features
         in do
              _ <- support (VkPhysicalDeviceVulkan12Features.runtimeDescriptorArray f) "runtimeDescriptorArray"
              _ <- support (VkPhysicalDeviceVulkan12Features.descriptorBindingPartiallyBound f) "descriptorBindingPartiallyBound"
              _ <- support (VkPhysicalDeviceVulkan12Features.descriptorBindingSampledImageUpdateAfterBind f) "descriptorBindingSampledImageUpdateAfterBind"
              support True "Bindless Descriptors"
      (_, exts) <- Vk.enumerateDeviceExtensionProperties gpu Nothing
      r <- swapchainSupported exts
      if r && dynamicRenderingSupported exts
        then do
          qs <- Vk.getPhysicalDeviceQueueFamilyProperties gpu
          maybeQs <- findM ff (Nothing, Nothing) (V.indexed qs)
          return $ (,portabilitySubSetPresent exts) <$> maybeQs
        else return Nothing
      where
        ff (gfx, present) (index, q)
          | Vk.queueCount q <= 0 = continue (gfx, present)
          | otherwise =
              let i = fromIntegral index
                  isGfx = ((Vk.QUEUE_GRAPHICS_BIT .&. Vk.queueFlags q) /= zeroBits) && (Vk.queueCount q > 0)
                  isPresent = Vk.getPhysicalDeviceSurfaceSupportKHR gpu i surface
                  pick b = if b then Just i else Nothing
                  gfx2 = present <|> pick isGfx
                  present2 = maybe (pick <$> isPresent) (pure . Just) gfx
                  nxt (Just g) (Just p) = found (g, p)
                  nxt g p = continue (g, p)
               in present2 >>= nxt gfx2

        dynamicRenderingSupported = V.any ((== Vk.KHR_DYNAMIC_RENDERING_EXTENSION_NAME) . Vk.extensionName)

        swapchainSupported exts = do
          (_, formats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR gpu surface
          (_, modes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR gpu surface
          let isGood f = VkSurface.format f == imageFormat && VkSurface.colorSpace f == colorSpace
              swapChain =
                any ((== Vk.KHR_SWAPCHAIN_EXTENSION_NAME) . Vk.extensionName) exts
                  && V.any isGood formats
                  && V.any (== presentMode) modes
          return swapChain

        portabilitySubSetPresent = any ((== Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME) . Vk.extensionName)