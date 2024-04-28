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
import Control.Monad (when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.Managed (Managed, MonadIO (liftIO), managed, managed_, runManaged)
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as BS (readFile)
import Data.ByteString.Char8 qualified as BS (pack, unpack)
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
import Foreign.C (peekCAString)
import Foreign.Ptr (castFunPtr)
import Foreign.Storable (Storable (..), sizeOf)
import Foreign.Storable.Record qualified as Store
import Geomancy qualified as G
import SDL qualified
import SDL.Video.Vulkan qualified as SDL
import Utils
import Vulkan qualified as PhysicalDeviceVulkan12Features (PhysicalDeviceVulkan12Features (..))
import Vulkan qualified as Vk
import Vulkan qualified as VkApplicationInfo (ApplicationInfo (..))
import Vulkan qualified as VkBufferCopy (BufferCopy (..))
import Vulkan qualified as VkBufferCreateInfo (BufferCreateInfo (..))
import Vulkan qualified as VkBufferImageCopy (BufferImageCopy (..))
import Vulkan qualified as VkCommandBufferAllocateInfo (CommandBufferAllocateInfo (..))
import Vulkan qualified as VkCommandBufferBeginInfo (CommandBufferBeginInfo (..))
import Vulkan qualified as VkCommandPoolCreateInfo (CommandPoolCreateInfo (..))
import Vulkan qualified as VkDebugUtilsMessengerCreateInfoEXT (DebugUtilsMessengerCreateInfoEXT (..))
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
import Vulkan qualified as VkImageMemoryBarrier (ImageMemoryBarrier (..))
import Vulkan qualified as VkImageSubresourceLayers (ImageSubresourceLayers (..))
import Vulkan qualified as VkImageSubresourceRange (ImageSubresourceRange (..))
import Vulkan qualified as VkImageViewCreateInfo (ImageViewCreateInfo (..))
import Vulkan qualified as VkInstance (Instance (..))
import Vulkan qualified as VkInstanceCreateInfo (InstanceCreateInfo (..))
import Vulkan qualified as VkPhysicalDeviceDescriptorIndexingFeatures (PhysicalDeviceDescriptorIndexingFeatures (..))
import Vulkan qualified as VkPhysicalDeviceDynamicRenderingFeatures (PhysicalDeviceDynamicRenderingFeatures (..))
import Vulkan qualified as VkPhysicalDeviceFeatures (PhysicalDeviceFeatures (..))
import Vulkan qualified as VkPhysicalDeviceFeatures2 (PhysicalDeviceFeatures2 (..))
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
import Vulkan.Utils.Debug qualified as Vk
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

data LoadedTexture = LoadedTexture {pixelSize :: G.UVec2, size :: G.Vec2, image :: Vk.Image, view :: Vk.ImageView}

data Texture = Texture {index :: Word32, texture :: LoadedTexture}

main :: IO ()
main = runManaged $ do
  withSDL
  window <- withWindow windowWidth windowHeight
  vulkan <- withVulkan window
  _ <- withDebug vulkan
  surface <- withSurface window vulkan
  (gpu, gfx, present, portable) <- pickGPU vulkan surface >>= maybe (liftIO . throwError . userError $ "Suitable GPU not found") return
  props <- Vk.getPhysicalDeviceProperties gpu
  say "Vulkan" $ "Picked up " ++ show (Vk.deviceName props) ++ ", present queue " ++ show present ++ ", graphics queue " ++ show gfx
  say "Vulkan" "Creating device"
  device <- withDevice gpu present gfx portable
  say "Vulkan" "Device created"
  gfxQueue <- Vk.getDeviceQueue device gfx 0
  say "Vulkan" "Got graphics queue"
  presentQueue <- Vk.getDeviceQueue device present 0
  say "Vulkan" "Got present queue"
  say "Vulkan" "Creating swap chain"
  (swapchain, images, extent) <-
    withSwapchain
      gpu
      device
      surface
      gfx
      present
      windowWidth
      windowHeight
  say "Vulkan" "Creating vertex buffer"
  say "Vulkan" "Vertex Buffer created"
  commandPool <-
    let info =
          Vk.zero
            { VkCommandPoolCreateInfo.queueFamilyIndex = gfx,
              VkCommandPoolCreateInfo.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
            }
     in managed $ Vk.withCommandPool device info Nothing bracket
  say "Vulkan" "Created command pool"
  allocator <- withMemoryAllocator vulkan gpu device
  say "VMA" "Created allocator"
  -- let bigBufferSize = 1048576
  -- bigBuffer <- withGPUBuffer allocator bigBufferSize Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
  -- say "Vulkan" "Created vertex buffer"

  let size = 1048576
  vertexBuffer <- withGPUBuffer allocator size Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
  say "Vulkan" "Created vertex buffer"
  (vertextStagingBuffer, vertextStagingBufferPointer) <- withHostBuffer allocator size
  say "Vulkan" "Created staging vertex buffer"
  descSetLayout <- descriptorSetLayout device 17
  (pipeline, pipelineLayout) <- createPipeline device extent descSetLayout

  descPool <- descriptorPool device 1000
  descSet <- descriptorSet device descSetLayout descPool
  sampler <- sampler device
  textures <-
    let textures = ["checkerboard", "pointer", "image3", "image", "image4"] :: [String]
     in traverse (\tex -> let file = "textures/" ++ tex ++ ".png" in readTexture allocator device commandPool gfxQueue file) textures
  [background, pointer, texture1, texture2, texture3] <- bindTextures device descSet textures sampler
  say "Vulkan" "Recorded command buffers"
  imageAvailable <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
  renderFinished <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
  inFlight <-
    let info = Vk.zero {VkFenceCreateInfo.flags = Vk.FENCE_CREATE_SIGNALED_BIT}
     in managed $ Vk.withFence device info Nothing bracket

  say "Engine" "Show window"
  SDL.showWindow window
  SDL.raiseWindow window
  SDL.cursorVisible SDL.$= False
  say "Engine" "Entering the main loop"
  -- withImGui vulkan gpu device window gfxQueue commandPool gfxQueue
  commandBuffers <- createCommandBuffers device commandPool (fromIntegral $ V.length images)
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
  let waitForPrevDrawCallToFinish = Vk.waitForFences device [inFlight] True maxBound *> Vk.resetFences device [inFlight]
      frame dt t es w0 = do
        w1 <- world dt t es w0
        let verts = vertices $ sprites w1
            vertextCount = fromIntegral $ SV.length verts
        copyBuffer device commandPool gfxQueue vertexBuffer (vertextStagingBuffer, vertextStagingBufferPointer) verts
        waitForPrevDrawCallToFinish
        index <- acquireNextFrame device swapchain imageAvailable
        let (img, view) = images ! fromIntegral index
            cmd = commandBuffers ! fromIntegral index
            transitToPresentLayout =
              transitImageLayout
                cmd
                img
                Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                (Vk.AccessFlagBits 0)
                Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
                Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
            transitToRenderLayout =
              transitImageLayout
                cmd
                img
                (Vk.AccessFlagBits 0)
                Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                Vk.IMAGE_LAYOUT_UNDEFINED
                Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
                Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        -- ImGui.vulkanNewFrame
        -- ImGui.sdl2NewFrame
        -- ImGui.newFrame
        -- ImGui.showDemoWindow
        -- ImGui.render
        -- imguiData <- ImGui.getDrawData
        -- say "Engine" "Rendering frame BEGIN"
        Vk.useCommandBuffer cmd Vk.zero $ do
          Vk.cmdBindPipeline cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
          Vk.cmdBindVertexBuffers cmd 0 [vertexBuffer] [0]
          Vk.cmdBindDescriptorSets cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 [descSet] []
          transitToRenderLayout
          renderScene extent vertextCount view cmd
          -- say "Engine" "Rendering ImGUI BEGIN"
          -- renderImgui cmd view extent imguiData
          transitToPresentLayout
        -- say "Engine" "Rendering frame END"
        renderFrame cmd gfxQueue imageAvailable renderFinished inFlight
        presentFrame swapchain presentQueue index renderFinished
        Vk.deviceWaitIdle device $> w1
   in liftIO $ mainLoop world0 frame

sampler :: Vk.Device -> Managed Vk.Sampler
sampler device =
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

withImGui :: Vk.Instance -> Vk.PhysicalDevice -> Vk.Device -> SDL.Window -> Vk.Queue -> Vk.CommandPool -> Vk.Queue -> Managed ()
withImGui vulkan gpu device window queue cmdPool gfx =
  do
    pool <-
      let poolSizes = poolSize <$> types
          info =
            Vk.zero
              { VkDescriptorPoolCreateInfo.poolSizes = poolSizes,
                VkDescriptorPoolCreateInfo.maxSets = 1000,
                VkDescriptorPoolCreateInfo.flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
              }
       in managed $ Vk.withDescriptorPool device info Nothing bracket
    managed_ $ bracket_ (init pool) Ashkan2.vulkanShutdown
  where
    types =
      [ Vk.DESCRIPTOR_TYPE_SAMPLER,
        Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
        Vk.DESCRIPTOR_TYPE_SAMPLED_IMAGE,
        Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE,
        Vk.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER,
        Vk.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
        Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER,
        Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
        Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC,
        Vk.DESCRIPTOR_TYPE_INPUT_ATTACHMENT
      ]
    poolSize typ =
      Vk.zero
        { VkDescriptorPoolSize.descriptorCount = 1000,
          VkDescriptorPoolSize.type' = typ
        }
    init pool = do
      _ <- ImGui.createContext
      say "ImGui" "Created context"
      _ <- ImGui.sdl2InitForVulkan window
      say "ImGui" "Initialized for SDL2 Vulkan"
      Ashkan2.vulkanInit vulkan gpu device gfx pool
      _ <- submitNow device cmdPool queue (\cmd -> ImGui.vulkanCreateFontsTexture cmd $> ())
      say "ImGui" "Created fonts texture"
      ImGui.vulkanDestroyFontUploadObjects
      say "ImGui" "Destroyed font upload objects"

world :: (Monad io) => Word32 -> Word32 -> [SDL.Event] -> World -> io World
world dt _ es w@(World {pointer = pointer@Sprite {pos = p}, a = a, b = b, c = c}) = return w {pointer = pointer {pos = foldl f p es}, a = update a, b = update b, c = update c}
  where
    f _ (SDL.Event _ (SDL.MouseMotionEvent (SDL.MouseMotionEventData {mouseMotionEventPos = SDL.P (SDL.V2 x y)}))) = normalPos x y
    f p _ = p
    update Thingie {sprite = sprite@Sprite {pos = p0}, vel = v0} =
      let p1 = p0 + (v0 G.^* fromIntegral dt)
          v1 = G.emap2 (\vi pi -> if pi >= 1.0 || pi <= -1.0 then -vi else vi) v0 p1
       in Thingie {sprite = sprite {pos = p1}, vel = v1}

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

acquireNextFrame :: (MonadIO io) => Vk.Device -> Vk.SwapchainKHR -> Vk.Semaphore -> io Word32
acquireNextFrame dev swapchain imageAvailable = do
  (r, index) <-
    Vk.acquireNextImageKHR
      dev
      swapchain
      maxBound
      imageAvailable
      Vk.zero
  when (r == Vk.SUBOPTIMAL_KHR || r == Vk.ERROR_OUT_OF_DATE_KHR) $ say "acquireNextFrame" $ show r
  return index

renderFrame :: (MonadIO io) => Vk.CommandBuffer -> Vk.Queue -> Vk.Semaphore -> Vk.Semaphore -> Vk.Fence -> io ()
renderFrame cmd gfx imageAvailable renderFinished inFlight =
  let info =
        Vk.zero
          { VkSubmitInfo.waitSemaphores = [imageAvailable],
            VkSubmitInfo.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT],
            VkSubmitInfo.commandBuffers = [Vk.commandBufferHandle cmd],
            VkSubmitInfo.signalSemaphores = [renderFinished]
          }
   in Vk.queueSubmit gfx [Vk.SomeStruct info] inFlight

presentFrame :: (MonadIO io) => Vk.SwapchainKHR -> Vk.Queue -> Word32 -> Vk.Semaphore -> io ()
presentFrame swapchain present idx renderFinished = do
  r <-
    let info =
          Vk.zero
            { VkPresentInfoKHR.waitSemaphores = [renderFinished],
              VkPresentInfoKHR.swapchains = [swapchain],
              VkPresentInfoKHR.imageIndices = [idx]
            }
     in Vk.queuePresentKHR present info
  when (r == Vk.SUBOPTIMAL_KHR || r == Vk.ERROR_OUT_OF_DATE_KHR) $ say "presentFrame" $ show r

sprites :: World -> [Sprite]
sprites (World background pointer Thingie {sprite = s1} Thingie {sprite = s2} Thingie {sprite = s3}) =
  [ background,
    s1,
    s2,
    s3,
    pointer
  ]

vulkanVersion :: Word32
vulkanVersion = Vk.API_VERSION_1_2

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

withDebug :: Vk.Instance -> Managed ()
withDebug vulkan = do
  _ <- managed $ Vk.withDebugUtilsMessengerEXT vulkan debugUtilsMessengerCreateInfo Nothing bracket
  Vk.submitDebugUtilsMessageEXT
    vulkan
    Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
    Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
    Vk.zero {Vk.message = "Debug Message Test"}

debugUtilsMessengerCreateInfo :: Vk.DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo =
  Vk.zero
    { Vk.messageSeverity =
        Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
      -- .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT,
      Vk.messageType =
        Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
      VkDebugUtilsMessengerCreateInfoEXT.pfnUserCallback = Vk.debugCallbackPtr
    }

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

copyBuffer ::
  forall a.
  (Storable a) =>
  Vk.Device ->
  Vk.CommandPool ->
  Vk.Queue ->
  Vk.Buffer ->
  (Vk.Buffer, Ptr ()) ->
  SV.Vector a ->
  IO ()
copyBuffer device pool queue gpuBuffer (hostBuffer, hostBufferPtr) v = do
  let (src, len) = SV.unsafeToForeignPtr0 v
      size = fromIntegral $ sizeOf (undefined :: a) * len
  liftIO . withForeignPtr src $ \s -> copyArray (castPtr hostBufferPtr) s len
  let copy cmd = Vk.cmdCopyBuffer cmd hostBuffer gpuBuffer [Vk.zero {VkBufferCopy.size = size}]
   in submitNow device pool queue copy

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
  let size = G.uvec2 (fromIntegral width) (fromIntegral height)
   in (\v -> LoadedTexture {pixelSize = size, size = normalSize size, image = image, view = v}) <$> withImageView device image Vk.FORMAT_R8G8B8A8_SRGB
  where
    copy pixels mem size =
      let (src, _) = SV.unsafeToForeignPtr0 pixels
          dst = castPtr mem
       in withForeignPtr src $ \src -> copyArray dst src size

submitNow :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> (Vk.CommandBuffer -> IO ()) -> IO ()
submitNow device pool queue f = buffer $ \buffs ->
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
  submitNow device pool queue $ \cmd -> do
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
  say "Vulkan" $ "Created buffer on host of size " ++ show size ++ " bytes"
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
  say "Vulkan" $ "Created buffer on GPU of size " ++ show size ++ " bytes"
  return buffer

mainLoop :: (MonadIO io) => s -> (Word32 -> Word32 -> [SDL.Event] -> s -> io s) -> io ()
mainLoop s draw = go s 0
  where
    go s0 t0 = do
      -- es <- ImGui.pollEventsWithImGui
      es <- SDL.pollEvents
      let quit = any isQuitEvent es
      if quit
        then pure ()
        else do
          t <- SDL.ticks
          s1 <- draw (t - t0) t es s0
          liftIO . threadDelay $ 1000 * 10
          go s1 t

isQuitEvent :: SDL.Event -> Bool
isQuitEvent = \case
  (SDL.Event _ SDL.QuitEvent) -> True
  SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released False (SDL.Keysym _ code _)))
    | code == SDL.KeycodeQ || code == SDL.KeycodeEscape ->
        True
  _ -> False

renderScene ::
  (MonadIO io) =>
  Vk.Extent2D ->
  Word32 ->
  Vk.ImageView ->
  Vk.CommandBuffer ->
  io ()
renderScene extent vertextCount target cmd = do
  let clear = Vk.Color (Vk.Float32 1.0 0.0 1.0 0)
      attachment =
        Vk.zero
          { VkRenderingAttachmentInfo.imageView = target,
            VkRenderingAttachmentInfo.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
            VkRenderingAttachmentInfo.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
            VkRenderingAttachmentInfo.storeOp = Vk.ATTACHMENT_STORE_OP_STORE,
            VkRenderingAttachmentInfo.clearValue = clear
          }
      scissor = Vk.Rect2D {VkRect2D.offset = Vk.Offset2D 0 0, VkRect2D.extent = extent}
      info =
        Vk.zero
          { VkRenderingInfo.renderArea = scissor,
            VkRenderingInfo.layerCount = 1,
            VkRenderingInfo.colorAttachments = [attachment]
          }
      -- drawScene = Vk.cmdDrawIndexed cmd indexCount 1 0 0 0
      drawScene = Vk.cmdDraw cmd (vertextCount + 12) 1 0 0
   in Vk.cmdUseRendering cmd info drawScene

renderImgui cmd view extent imguiData =
  let attachment =
        Vk.zero
          { VkRenderingAttachmentInfo.imageView = view,
            VkRenderingAttachmentInfo.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
            VkRenderingAttachmentInfo.loadOp = Vk.ATTACHMENT_LOAD_OP_LOAD,
            VkRenderingAttachmentInfo.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
          }
      scissor = Vk.Rect2D {VkRect2D.offset = Vk.Offset2D 0 0, VkRect2D.extent = extent}
      info =
        Vk.zero
          { VkRenderingInfo.renderArea = scissor,
            VkRenderingInfo.layerCount = 1,
            VkRenderingInfo.colorAttachments = [attachment]
          }
   in Vk.cmdUseRendering cmd info $ ImGui.vulkanRenderDrawData imguiData cmd Nothing

transitImageLayout ::
  (MonadIO io) =>
  Vk.CommandBuffer ->
  Vk.Image ->
  Vk.AccessFlagBits ->
  Vk.AccessFlagBits ->
  Vk.ImageLayout ->
  Vk.ImageLayout ->
  Vk.PipelineStageFlags ->
  Vk.PipelineStageFlags ->
  io ()
transitImageLayout cmd img srcMask dstMask old new srcStage dstStage =
  let range =
        Vk.zero
          { VkImageSubresourceRange.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
            VkImageSubresourceRange.baseMipLevel = 0,
            VkImageSubresourceRange.levelCount = 1,
            VkImageSubresourceRange.baseArrayLayer = 0,
            VkImageSubresourceRange.layerCount = 1
          }
      barrier =
        Vk.zero
          { VkImageMemoryBarrier.srcAccessMask = srcMask,
            VkImageMemoryBarrier.dstAccessMask = dstMask,
            VkImageMemoryBarrier.oldLayout = old,
            VkImageMemoryBarrier.newLayout = new,
            VkImageMemoryBarrier.image = img,
            VkImageMemoryBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkImageMemoryBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkImageMemoryBarrier.subresourceRange = range
          }
   in Vk.cmdPipelineBarrier cmd srcStage dstStage (Vk.DependencyFlagBits 0) [] [] [Vk.SomeStruct barrier]

createCommandBuffers ::
  Vk.Device ->
  Vk.CommandPool ->
  Word32 ->
  Managed (V.Vector Vk.CommandBuffer)
createCommandBuffers device pool count = do
  say "Vulkan" "Created pipeline"
  commandBuffers <-
    let info =
          Vk.zero
            { Vk.commandPool = pool,
              Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
              Vk.commandBufferCount = count
            }
     in managed $ Vk.withCommandBuffers device info bracket
  say "Vulkan" "Created comamand buffers"
  return commandBuffers

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
              VkWriteDescriptorSet.imageInfo = V.fromList $ imageInfo . view <$> textures,
              VkWriteDescriptorSet.dstArrayElement = 0
            }
   in Vk.updateDescriptorSets dev [info] []
  return $ zipWith update [0 ..] textures
  where
    update i tex = Texture {index = i, texture = tex}
    view LoadedTexture {view = v} = v
    imageInfo tex =
      Vk.zero
        { VkDescriptorImageInfo.imageView = tex,
          VkDescriptorImageInfo.sampler = sampler,
          VkDescriptorImageInfo.imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        }

createPipeline ::
  Vk.Device ->
  Vk.Extent2D ->
  Vk.DescriptorSetLayout ->
  Managed (Vk.Pipeline, Vk.PipelineLayout)
createPipeline dev extent setLayout = do
  pipelineLayout <-
    let info =
          Vk.zero
            { VkPipelineLayoutCreateInfo.setLayouts = [setLayout]
            }
     in managed $ Vk.withPipelineLayout dev info Nothing bracket
  (vert, frag) <- createShaders dev
  (_, res) <-
    let xySize = sizeOf (undefined :: G.Vec2)
        rgbSize = sizeOf (undefined :: G.Vec3)
        tetCordSize = sizeOf (undefined :: G.Vec2)
        position =
          Vk.zero
            { VkVertexInputAttributeDescription.binding = 0,
              VkVertexInputAttributeDescription.location = 0,
              VkVertexInputAttributeDescription.format = Vk.FORMAT_R32G32_SFLOAT,
              VkVertexInputAttributeDescription.offset = 0
            }
        color =
          let offset = xySize
           in Vk.zero
                { VkVertexInputAttributeDescription.binding = 0,
                  VkVertexInputAttributeDescription.location = 1,
                  VkVertexInputAttributeDescription.format = Vk.FORMAT_R32G32B32_SFLOAT,
                  VkVertexInputAttributeDescription.offset = fromIntegral offset
                }
        textureCoordinates =
          let offset = xySize + rgbSize
           in Vk.zero
                { VkVertexInputAttributeDescription.binding = 0,
                  VkVertexInputAttributeDescription.location = 2,
                  VkVertexInputAttributeDescription.format = Vk.FORMAT_R32G32_SFLOAT,
                  VkVertexInputAttributeDescription.offset = fromIntegral offset
                }
        texttureIndex =
          let offset = xySize + rgbSize + tetCordSize
           in Vk.zero
                { VkVertexInputAttributeDescription.binding = 0,
                  VkVertexInputAttributeDescription.location = 3,
                  VkVertexInputAttributeDescription.format = Vk.FORMAT_R32_UINT,
                  VkVertexInputAttributeDescription.offset = fromIntegral offset
                }
        attributes = [position, color, textureCoordinates, texttureIndex]
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
              VkGraphicsPipelineCreateInfo.layout = pipelineLayout,
              VkGraphicsPipelineCreateInfo.subpass = 0,
              VkGraphicsPipelineCreateInfo.basePipelineHandle = Vk.zero
            }
            ::& dynamicRendering :& ()
     in managed $ Vk.withGraphicsPipelines dev Vk.zero [Vk.SomeStruct pipelineCreateInfo] Nothing bracket
  return . (,pipelineLayout) . V.head $ res

framesInFlight :: Word32
framesInFlight = 1

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

withSwapchain ::
  Vk.PhysicalDevice ->
  Vk.Device ->
  Vk.SurfaceKHR ->
  Word32 ->
  Word32 ->
  Int ->
  Int ->
  Managed (Vk.SwapchainKHR, V.Vector (Vk.Image, Vk.ImageView), Vk.Extent2D)
withSwapchain
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
      viewImages <-
        let f img = withImageView dev img imageFormat
         in for images $ \img -> (img,) <$> f img
      return (swapchain, viewImages, extent)

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
          { PhysicalDeviceVulkan12Features.descriptorIndexing = True,
            PhysicalDeviceVulkan12Features.descriptorBindingPartiallyBound = True,
            PhysicalDeviceVulkan12Features.shaderSampledImageArrayNonUniformIndexing = True,
            PhysicalDeviceVulkan12Features.descriptorBindingSampledImageUpdateAfterBind = True
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
      features2 <- Vk.getPhysicalDeviceFeatures2 gpu :: Managed (Vk.PhysicalDeviceFeatures2 '[Vk.PhysicalDeviceDescriptorIndexingFeatures])
      let features = VkPhysicalDeviceFeatures2.features features2
      _ <- support (VkPhysicalDeviceFeatures.shaderSampledImageArrayDynamicIndexing features) "shaderSampledImageArrayDynamicIndexing"
      _ <-
        let f = fst $ VkPhysicalDeviceFeatures2.next features2
         in do
              _ <- support (VkPhysicalDeviceDescriptorIndexingFeatures.descriptorBindingPartiallyBound f) "descriptorBindingPartiallyBound"
              _ <- support (VkPhysicalDeviceDescriptorIndexingFeatures.shaderSampledImageArrayNonUniformIndexing f) "shaderSampledImageArrayNonUniformIndexing"
              _ <- support (VkPhysicalDeviceDescriptorIndexingFeatures.descriptorBindingSampledImageUpdateAfterBind f) "descriptorBindingSampledImageUpdateAfterBind"
              support True "GPU Bindless Descriptors"
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

withVulkan :: SDL.Window -> Managed Vk.Instance
withVulkan w = do
  reqExts <- mapM (fmap BS.pack . peekCAString) <$> SDL.vkGetInstanceExtensions w
  instExts <-
    liftIO $
      let extra =
            [ Vk.EXT_DEBUG_UTILS_EXTENSION_NAME,
              "VK_EXT_layer_settings",
              Vk.KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
            ]
       in (++ extra) <$> reqExts
  say "Vulkan" $ "Instance extenions: " ++ unwords (BS.unpack <$> instExts)
  let instanceCreateInfo =
        Vk.zero
          { VkInstanceCreateInfo.applicationInfo =
              Just
                Vk.zero
                  { VkApplicationInfo.applicationName = Just "Vulkan",
                    VkApplicationInfo.apiVersion = vulkanVersion
                  },
            VkInstanceCreateInfo.enabledExtensionNames = V.fromList instExts,
            VkInstanceCreateInfo.enabledLayerNames = ["VK_LAYER_KHRONOS_validation"],
            VkInstanceCreateInfo.flags = Vk.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
          }
          ::& debugUtilsMessengerCreateInfo
            :& Vk.ValidationFeaturesEXT
              [ -- Vk.VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT,
                Vk.VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT -- ,
                -- Vk.VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT
              ]
              []
            :& ()
  managed $ Vk.withInstance instanceCreateInfo Nothing bracket

withSurface :: SDL.Window -> Vk.Instance -> Managed Vk.SurfaceKHR
withSurface w v@(Vk.Instance v' _) =
  managed $
    bracket
      (putStrLn "SDL: Creating Vulkan surface" *> create <* putStrLn "SDL: Vulkan surface created")
      (\s -> Vk.destroySurfaceKHR v s Nothing <* putStrLn "Vulkan: Destroyed surface")
  where
    create = Vk.SurfaceKHR <$> SDL.vkCreateSurface w (castPtr v')

withWindow :: Int -> Int -> Managed SDL.Window
withWindow width height =
  managed $
    bracket
      (say "SDL" "Creating window" *> create <* say "SDL" "Window created")
      (\w -> say "SDL" "Window destroyed" *> SDL.destroyWindow w)
  where
    create =
      SDL.createWindow
        "Vulkan"
        ( SDL.defaultWindow
            { SDL.windowInitialSize =
                SDL.V2
                  (fromIntegral width)
                  (fromIntegral height),
              SDL.windowGraphicsContext = SDL.VulkanContext,
              SDL.windowPosition = SDL.Centered
            }
        )

withSDL :: Managed ()
withSDL =
  managed_ $
    bracket_
      (ver *> init <* say "SDL" "Initialized")
      (SDL.quit *> say "SDL" "Quit")
      . withVkLib
  where
    ver = SDL.version >>= (\(v0 :: Int, v1, v2) -> putStrLn $ "SDL: Version " ++ show v0 ++ "." ++ show v1 ++ "." ++ show v2)
    init = SDL.initialize ([SDL.InitEvents, SDL.InitVideo] :: [SDL.InitFlag])
    withVkLib = bracket_ (SDL.vkLoadLibrary Nothing) SDL.vkUnloadLibrary