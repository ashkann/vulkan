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

import Ashkan2 qualified
import Codec.Picture qualified as JP
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import Control.Monad (forM, replicateM, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Managed (Managed, MonadIO (liftIO), managed, managed_, runManaged)
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as BS (readFile)
import Data.Foldable (foldlM)
import Data.Functor (($>), (<&>))
import Data.Traversable (for)
import Data.Vector ((!))
import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import DearImGui qualified as ImGui
import DearImGui.SDL.Vulkan qualified as ImGui
import DearImGui.Vulkan qualified as ImGui
import Foreign (Bits (zeroBits), Ptr, Storable, Word32, castPtr, copyArray, withForeignPtr)
import Foreign.Ptr (castFunPtr)
import Foreign.Storable (Storable (..), sizeOf)
import Foreign.Storable.Record qualified as Store
import Geomancy qualified as G
import SDL qualified
import Utils
import Vulkan qualified as Vk
import Vulkan qualified as VkBufferCopy (BufferCopy (..))
import Vulkan qualified as VkBufferCreateInfo (BufferCreateInfo (..))
import Vulkan qualified as VkBufferImageCopy (BufferImageCopy (..))
import Vulkan qualified as VkCommandBufferAllocateInfo (CommandBufferAllocateInfo (..))
import Vulkan qualified as VkCommandBufferBeginInfo (CommandBufferBeginInfo (..))
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
import Vulkan qualified as VkImageSubresourceLayers (ImageSubresourceLayers (..))
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

data Vertex = Vertex {xy :: G.Vec2, rgb :: G.Vec3, uv :: G.Vec2, texture :: Word32}
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
    texture :: Texture
  }

data Thingie = Thingie {sprite :: Sprite, vel :: G.Vec2}

data World = World {background :: Sprite, pointer :: Sprite, a :: Thingie, b :: Thingie, c :: Thingie}

data LoadedTexture = LoadedTexture {resolution :: G.UVec2, size :: G.Vec2, image :: Vk.Image, view :: Vk.ImageView}

data Texture = Texture {index :: Word32, texture :: LoadedTexture}

data Frame = Frame
  { cmd :: Vk.CommandBuffer,
    fence :: Vk.Fence,
    imageAvailable :: Vk.Semaphore,
    renderFinished :: Vk.Semaphore,
    staging :: (Vk.Buffer, Ptr ()),
    vertex :: Vk.Buffer
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
    let textures = ["checkerboard", "pointer", "image3", "image", "image4"] :: [String]
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
      world0 =
        World
          { background = Sprite {pos = G.vec2 (-1.0) (-1.0), scale = G.vec2 0.2 0.2, texture = background},
            pointer = Sprite {pos = G.vec2 0.0 0.0, texture = pointer, scale = half},
            a = Thingie {sprite = Sprite {pos = p0, scale = third, texture = texture1}, vel = G.vec2 0.002 0.001},
            b = Thingie {sprite = Sprite {pos = p0, scale = third, texture = texture2}, vel = G.vec2 0.0 0.0},
            c = Thingie {sprite = Sprite {pos = p0, scale = third, texture = texture3}, vel = G.vec2 0.001 0.002}
          }

  swapchain@(_, swapchainExtent, swapchainImagesAndViews) <-
    createSwapchain
      gpu
      device
      surface
      gfx
      present
      windowWidth
      windowHeight
  let frameCount = V.length swapchainImagesAndViews
  say "Engin" $ "Frame count is " ++ show frameCount

  pipelineLayout <-
    let info = Vk.zero {VkPipelineLayoutCreateInfo.setLayouts = [descSetLayout]}
     in managed $ Vk.withPipelineLayout device info Nothing bracket
  pipeline <- createPipeline device swapchainExtent pipelineLayout
  presentQueue <- Vk.getDeviceQueue device present 0 <* say "Vulkan" "Got present queue"

  let stagingBufferSize = 1048576
      vertexBufferSize = 4 * 1024
  frames <- withFrames device commandPool allocator stagingBufferSize vertexBufferSize frameCount
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
                 in frame device commandPool gfxQueue presentQueue pipeline pipelineLayout swapchain descSet f verts
          )
          worldTime
          worldEvent
          world0

frame ::
  (Storable a, MonadIO m) =>
  Vk.Device ->
  Vk.CommandPool ->
  Vk.Queue ->
  Vk.Queue ->
  Vk.Pipeline ->
  Vk.PipelineLayout ->
  (Vk.SwapchainKHR, VkExtent2D.Extent2D, V.Vector (Vk.Image, Vk.ImageView)) ->
  Vk.DescriptorSet ->
  Frame ->
  SV.Vector a ->
  m ()
frame device commandPool gfxQueue presentQueue pipeline pipelineLayout (swapchain, swapchainExtent, imagesAndViews) descSet f@(Frame {cmd, fence, imageAvailable, renderFinished, staging, vertex}) verts = do
  waitForFrame device f
  liftIO $ copyToGpu device commandPool gfxQueue vertex staging verts

  (r, index) <- Vk.acquireNextImageKHR device swapchain maxBound imageAvailable Vk.zero
  when (r == Vk.SUBOPTIMAL_KHR || r == Vk.ERROR_OUT_OF_DATE_KHR) $ say "Engine" $ "acquireNextFrame = " ++ show r
  let (image, view) = imagesAndViews ! fromIntegral index
  Vk.useCommandBuffer cmd Vk.zero $ do
    Vk.cmdBindPipeline cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
    Vk.cmdBindVertexBuffers cmd 0 [vertex] [0]
    Vk.cmdBindDescriptorSets cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 [descSet] []
    transitToRenderLayout cmd image
    let vertexCount = fromIntegral $ SV.length verts
        attachment =
          Vk.zero
            { VkRenderingAttachmentInfo.imageView = view,
              VkRenderingAttachmentInfo.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
              VkRenderingAttachmentInfo.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
              VkRenderingAttachmentInfo.storeOp = Vk.ATTACHMENT_STORE_OP_STORE,
              VkRenderingAttachmentInfo.clearValue = clearColor
            }
        scissor = Vk.Rect2D {VkRect2D.offset = Vk.Offset2D 0 0, VkRect2D.extent = swapchainExtent}
        info =
          Vk.zero
            { VkRenderingInfo.renderArea = scissor,
              VkRenderingInfo.layerCount = 1,
              VkRenderingInfo.colorAttachments = [attachment]
            }
        draw = Vk.cmdDraw cmd vertexCount 1 0 0
     in Vk.cmdUseRendering cmd info draw
    transitToPresentLayout cmd image
  let info =
        [ Vk.SomeStruct
            Vk.zero
              { VkSubmitInfo.waitSemaphores = [imageAvailable],
                VkSubmitInfo.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT],
                VkSubmitInfo.commandBuffers = [Vk.commandBufferHandle cmd],
                VkSubmitInfo.signalSemaphores = [renderFinished]
              }
        ]
   in Vk.queueSubmit gfxQueue info fence
  r2 <-
    let info =
          Vk.zero
            { VkPresentInfoKHR.waitSemaphores = [renderFinished],
              VkPresentInfoKHR.swapchains = [swapchain],
              VkPresentInfoKHR.imageIndices = [index]
            }
     in Vk.queuePresentKHR presentQueue info
  when (r2 == Vk.SUBOPTIMAL_KHR || r2 == Vk.ERROR_OUT_OF_DATE_KHR) (say "Engine" $ "presentFrame" ++ show r)

withFrames ::
  VkDevice.Device ->
  Vk.CommandPool ->
  Vma.Allocator ->
  Vk.DeviceSize ->
  Vk.DeviceSize ->
  Int ->
  Managed (V.Vector Frame)
withFrames device commandPool allocator stagingBufferSize vertexBufferSize frameCount =
  do
    cmds <-
      let info =
            Vk.zero
              { Vk.commandPool = commandPool,
                Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
                Vk.commandBufferCount = fromIntegral frameCount
              }
       in managed $ Vk.withCommandBuffers device info bracket
    forM cmds singleFrame
  where
    singleFrame cmd =
      do
        vertextStagingBuffer <- withHostBuffer allocator stagingBufferSize
        imageAvailable <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
        renderFinished <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
        fence <-
          let info = Vk.zero {VkFenceCreateInfo.flags = Vk.FENCE_CREATE_SIGNALED_BIT}
           in managed $ Vk.withFence device info Nothing bracket
        vertexBuffer <- withGPUBuffer allocator vertexBufferSize Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
        return
          Frame
            { cmd = cmd,
              fence = fence,
              imageAvailable = imageAvailable,
              renderFinished = renderFinished,
              staging = vertextStagingBuffer,
              vertex = vertexBuffer
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
worldTime dt w@(World {a = a, b = b, c = c}) = return w {a = update a, b = update b, c = update c}
  where
    update Thingie {sprite = sprite@Sprite {pos = p0}, vel = v0} =
      let p1 = p0 + (v0 G.^* fromIntegral dt)
          v1 = G.emap2 (\vi pos -> if pos >= 1.0 || pos <= -1.0 then -vi else vi) v0 p1
       in Thingie {sprite = sprite {pos = p1}, vel = v1}

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

-- withImGui :: Vk.Instance -> Vk.PhysicalDevice -> Vk.Device -> SDL.Window -> Vk.Queue -> Vk.CommandPool -> Vk.Queue -> Managed ()
-- withImGui vulkan gpu device window queue cmdPool gfx =
--   do
--     pool <-
--       let poolSizes = poolSize <$> types
--           info =
--             Vk.zero
--               { VkDescriptorPoolCreateInfo.poolSizes = poolSizes,
--                 VkDescriptorPoolCreateInfo.maxSets = 1000,
--                 VkDescriptorPoolCreateInfo.flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
--               }
--        in managed $ Vk.withDescriptorPool device info Nothing bracket
--     managed_ $ bracket_ (init pool) Ashkan2.vulkanShutdown
--   where
--     types =
--       [ Vk.DESCRIPTOR_TYPE_SAMPLER,
--         Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
--         Vk.DESCRIPTOR_TYPE_SAMPLED_IMAGE,
--         Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE,
--         Vk.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER,
--         Vk.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
--         Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
--         Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER,
--         Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
--         Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC,
--         Vk.DESCRIPTOR_TYPE_INPUT_ATTACHMENT
--       ]
--     poolSize typ =
--       Vk.zero
--         { VkDescriptorPoolSize.descriptorCount = 1000,
--           VkDescriptorPoolSize.type' = typ
--         }
--     init pool = do
--       _ <- ImGui.createContext
--       say "ImGui" "Created context"
--       _ <- ImGui.sdl2InitForVulkan window
--       say "ImGui" "Initialized for SDL2 Vulkan"
--       Ashkan2.vulkanInit vulkan gpu device gfx pool
--       _ <- submitNow device cmdPool queue (\cmd -> ImGui.vulkanCreateFontsTexture cmd $> ())
--       say "ImGui" "Created fonts texture"
--       ImGui.vulkanDestroyFontUploadObjects
--       say "ImGui" "Destroyed font upload objects"

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
sprites (World background pointer Thingie {sprite = s1} Thingie {sprite = s2} Thingie {sprite = s3}) =
  [ background,
    s1,
    s2,
    s3,
    pointer
  ]

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
      toQuad Sprite {pos = G.WithVec2 x y, scale = G.WithVec2 sx sy, texture = Texture {index = tex, texture = LoadedTexture {size = G.WithVec2 w h}}} =
        let topLeft = Vertex {xy = G.vec2 x y, rgb = rgb, uv = G.vec2 0.0 0.0, texture = tex}
            topRight = Vertex {xy = G.vec2 (x + w * sx) y, rgb = rgb, uv = G.vec2 1.0 0.0, texture = tex}
            bottomRight = Vertex {xy = G.vec2 (x + w * sx) (y + h * sy), rgb = rgb, uv = G.vec2 1.0 1.0, texture = tex}
            bottomLeft = Vertex {xy = G.vec2 x (y + h * sy), rgb = rgb, uv = G.vec2 0.0 1.0, texture = tex}
         in SV.fromList
              [topLeft, topRight, bottomRight, bottomRight, bottomLeft, topLeft]
   in mconcat $ toQuad <$> ss

copyToGpu ::
  forall a.
  (Storable a) =>
  Vk.Device ->
  Vk.CommandPool ->
  Vk.Queue ->
  Vk.Buffer ->
  (Vk.Buffer, Ptr ()) ->
  SV.Vector a ->
  IO ()
copyToGpu device pool queue gpuBuffer (hostBuffer, hostBufferPtr) v = do
  let (src, len) = SV.unsafeToForeignPtr0 v
      size = fromIntegral $ sizeOf (undefined :: a) * len
  withForeignPtr src $ \s -> copyArray (castPtr hostBufferPtr) s len
  let copy cmd = Vk.cmdCopyBuffer cmd hostBuffer gpuBuffer [Vk.zero {VkBufferCopy.size = size}]
   in submitWait device pool queue copy

withImage :: Vma.Allocator -> Int -> Int -> Managed Vk.Image
withImage allocator width height = do
  (image, _, _) <-
    let dims =
          Vk.Extent3D
            { VkExtent3D.width = fromIntegral width,
              VkExtent3D.height = fromIntegral height,
              VkExtent3D.depth = 1
            }
        usage = Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT .|. Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        imgInfo =
          Vk.zero
            { VkImageCreateInfo.imageType = Vk.IMAGE_TYPE_2D,
              VkImageCreateInfo.extent = dims,
              VkImageCreateInfo.mipLevels = 1,
              VkImageCreateInfo.arrayLayers = 1,
              VkImageCreateInfo.format = Vk.FORMAT_R8G8B8A8_SRGB,
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

readTexture :: Vma.Allocator -> Vk.Device -> Vk.CommandPool -> Vk.Queue -> FilePath -> Managed LoadedTexture
readTexture allocator device pool queue path = do
  JP.ImageRGBA8 (JP.Image width height pixels) <- liftIO $ JP.readPng path >>= either (sayErr "Texture" . show) return
  let size = width * height * 4
  image <- withImage allocator width height
  do
    (staging, mem) <- withHostBuffer allocator (fromIntegral size)
    liftIO $ copy pixels mem size
    copyBufferToImage device pool queue staging image width height
  let res = G.uvec2 (fromIntegral width) (fromIntegral height)
      tex v = LoadedTexture {resolution = res, size = normalSize res, image = image, view = v}
   in tex <$> withImageView device image Vk.FORMAT_R8G8B8A8_SRGB
  where
    copy pixels mem size =
      let (src, _) = SV.unsafeToForeignPtr0 pixels
          dst = castPtr mem
       in withForeignPtr src $ \from -> copyArray dst from size

submitWait :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> (Vk.CommandBuffer -> IO ()) -> IO ()
submitWait device pool queue f = buffer $ \buffs ->
  do
    let buff = V.head buffs
    let info = Vk.zero {VkCommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
     in Vk.useCommandBuffer buff info $ f buff
    let work = Vk.SomeStruct $ Vk.zero {VkSubmitInfo.commandBuffers = [Vk.commandBufferHandle buff]}
     in Vk.queueSubmit queue [work] Vk.NULL_HANDLE
    Vk.queueWaitIdle queue
  where
    buffer =
      let info =
            Vk.zero
              { VkCommandBufferAllocateInfo.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
                VkCommandBufferAllocateInfo.commandPool = pool,
                VkCommandBufferAllocateInfo.commandBufferCount = 1
              }
       in Vk.withCommandBuffers device info bracket

copyBufferToImage ::
  (MonadIO io) =>
  VkDevice.Device ->
  Vk.CommandPool ->
  Vk.Queue ->
  Vk.Buffer ->
  Vk.Image ->
  Int ->
  Int ->
  io ()
copyBufferToImage device pool queue src dst width height = liftIO $ do
  let subresource =
        Vk.zero
          { VkImageSubresourceLayers.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
            VkImageSubresourceLayers.layerCount = 1,
            VkImageSubresourceLayers.mipLevel = 0,
            VkImageSubresourceLayers.baseArrayLayer = 0
          }
      dims =
        Vk.Extent3D
          { VkExtent3D.width = fromIntegral width,
            VkExtent3D.height = fromIntegral height,
            VkExtent3D.depth = 1
          }
      region =
        Vk.zero
          { VkBufferImageCopy.bufferOffset = 0,
            VkBufferImageCopy.bufferRowLength = 0,
            VkBufferImageCopy.bufferImageHeight = 0,
            VkBufferImageCopy.imageSubresource = subresource,
            VkBufferImageCopy.imageExtent = dims
          }
  submitWait device pool queue $ \cmd -> do
    tranistToDstOptimal cmd dst
    Vk.cmdCopyBufferToImage cmd src dst Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL [region]
    tranistDstOptimalToShaderReadOnlyOptimal cmd dst

tranistToDstOptimal :: (MonadIO io) => Vk.CommandBuffer -> Vk.Image -> io ()
tranistToDstOptimal cmd img =
  transitImageLayout
    cmd
    img
    (Vk.AccessFlagBits 0)
    Vk.ACCESS_TRANSFER_WRITE_BIT
    Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
    Vk.PIPELINE_STAGE_TRANSFER_BIT

tranistDstOptimalToShaderReadOnlyOptimal :: (MonadIO io) => Vk.CommandBuffer -> Vk.Image -> io ()
tranistDstOptimalToShaderReadOnlyOptimal cmd img =
  transitImageLayout
    cmd
    img
    Vk.ACCESS_TRANSFER_WRITE_BIT
    Vk.ACCESS_SHADER_READ_BIT
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT

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

-- renderImgui cmd view extent imguiData =
--   let attachment =
--         Vk.zero
--           { VkRenderingAttachmentInfo.imageView = view,
--             VkRenderingAttachmentInfo.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
--             VkRenderingAttachmentInfo.loadOp = Vk.ATTACHMENT_LOAD_OP_LOAD,
--             VkRenderingAttachmentInfo.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
--           }
--       scissor = Vk.Rect2D {VkRect2D.offset = Vk.Offset2D 0 0, VkRect2D.extent = extent}
--       info =
--         Vk.zero
--           { VkRenderingInfo.renderArea = scissor,
--             VkRenderingInfo.layerCount = 1,
--             VkRenderingInfo.colorAttachments = [attachment]
--           }
--    in Vk.cmdUseRendering cmd info $ ImGui.vulkanRenderDrawData imguiData cmd Nothing

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
  Managed (Vk.SwapchainKHR, Vk.Extent2D, V.Vector (Vk.Image, Vk.ImageView))
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
                VkSwapchainCreateInfo.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                VkSwapchainCreateInfo.imageSharingMode = sharingMode,
                VkSwapchainCreateInfo.queueFamilyIndices = queues,
                VkSwapchainCreateInfo.preTransform = transform,
                VkSwapchainCreateInfo.compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                VkSwapchainCreateInfo.presentMode = presentMode,
                VkSwapchainCreateInfo.clipped = True
              }
      swapchain <- managed $ Vk.withSwapchainKHR dev info Nothing bracket
      (_, images) <- Vk.getSwapchainImagesKHR dev swapchain
      imagesAndViews <- for images $ \image -> (image,) <$> withImageView dev image imageFormat
      return (swapchain, extent, imagesAndViews)

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