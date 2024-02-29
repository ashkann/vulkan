{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Codec.Picture qualified as JP
import Control.Applicative (liftA3, (<|>))
import Control.Exception (bracket, bracket_)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.Extra (whileM)
import Control.Monad.Managed (Managed, MonadIO (liftIO), MonadManaged, managed, managed_, runManaged)
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as BS (readFile)
import Data.ByteString.Char8 qualified as BS (pack, unpack)
import Data.Foldable (foldlM, for_)
import Data.Functor ((<&>))
import Data.Traversable (for)
import Data.Vector ((!))
import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import Foreign (Bits (zeroBits), Ptr, Storable, Word32, castPtr, copyArray, withForeignPtr)
import Foreign.C (peekCAString)
import Foreign.Ptr (castFunPtr)
import Foreign.Storable (Storable (..), sizeOf)
import Foreign.Storable.Record qualified as Store
import Geomancy qualified as G
import SDL qualified
import SDL.Video.Vulkan qualified as SDL
import Vulkan qualified as Vk
import Vulkan qualified as VkApplicationInfo (ApplicationInfo (..))
import Vulkan qualified as VkBufferCopy (BufferCopy (..))
import Vulkan qualified as VkBufferCreateInfo (BufferCreateInfo (..))
import Vulkan qualified as VkBufferImageCopy (BufferImageCopy (..))
import Vulkan qualified as VkCommandBufferAllocateInfo (CommandBufferAllocateInfo (..))
import Vulkan qualified as VkCommandBufferBeginInfo (CommandBufferBeginInfo (..))
import Vulkan qualified as VkCommandPoolCreateInfo (CommandPoolCreateInfo (..))
import Vulkan qualified as VkDebugUtilsMessengerCreateInfoEXT (DebugUtilsMessengerCreateInfoEXT (..))
import Vulkan qualified as VkDescriptorBufferInfo (DescriptorBufferInfo (..))
import Vulkan qualified as VkDescriptorImageInfo (DescriptorImageInfo (..))
import Vulkan qualified as VkDescriptorPoolCreateInfo (DescriptorPoolCreateInfo (..))
import Vulkan qualified as VkDescriptorPoolSize (DescriptorPoolSize (..))
import Vulkan qualified as VkDescriptorSetAllocateInfo (DescriptorSetAllocateInfo (..))
import Vulkan qualified as VkDescriptorSetLayoutBinding (DescriptorSetLayoutBinding (..))
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

data Vertex = Vertex {xy :: G.Vec2, rgb :: G.Vec3, uv :: G.Vec2}
  deriving (Show, Eq)

store :: Store.Dictionary Vertex
store =
  Store.run $
    liftA3
      Vertex
      (Store.element xy)
      (Store.element rgb)
      (Store.element uv)

instance Storable Vertex where
  sizeOf = Store.sizeOf store
  alignment = Store.alignment store
  peek = Store.peek store
  poke = Store.poke store

main :: IO ()
main = runManaged $ do
  let windowWidth = 500
      windowHeight = 500
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
  allocator <- withMemoryAllocator vulkan gpu device
  say "VMA" "Created allocator"
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
    let info = Vk.zero {VkCommandPoolCreateInfo.queueFamilyIndex = gfx}
     in managed $ Vk.withCommandPool device info Nothing bracket
  say "Vulkan" "Created command pool"
  vertexBuffer <-
    let vertecies =
          SV.fromList
            [ Vertex {xy = G.vec2 (-0.5) (-0.5), rgb = G.vec3 1.0 0.0 0.0, uv = G.vec2 0.0 0.0},
              Vertex {xy = G.vec2 0.5 (-0.5), rgb = G.vec3 0.0 1.0 0.0, uv = G.vec2 1.0 0.0},
              Vertex {xy = G.vec2 0.5 0.5, rgb = G.vec3 0.0 0.0 1.0, uv = G.vec2 1.0 1.0},
              Vertex {xy = G.vec2 (-0.5) 0.5, rgb = G.vec3 1.0 1.0 1.0, uv = G.vec2 0.0 1.0}
            ]
     in withBuffer allocator 1024 device commandPool gfxQueue Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT vertecies
  say "Vulkan" "Created vertex buffer"
  indexBuffer <-
    let indecies = SV.fromList [0, 1, 2, 2, 3, 0] :: SV.Vector Word32
     in withBuffer allocator 1025 device commandPool gfxQueue Vk.BUFFER_USAGE_INDEX_BUFFER_BIT indecies
  say "Vulkan" "Created index buffer"
  sampler <-
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
  textureImage <- texture allocator device commandPool gfxQueue "textures/image.png"
  say "Vulkan" $ "Created texture " ++ show textureImage
  commandBuffers <- createCommandBuffers device commandPool extent images vertexBuffer indexBuffer textureImage sampler
  imageAvailable <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
  renderFinished <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
  inFlight <-
    let info = Vk.zero {VkFenceCreateInfo.flags = Vk.FENCE_CREATE_SIGNALED_BIT}
     in managed $ Vk.withFence device info Nothing bracket

  say "Engine" "Show window"
  SDL.showWindow window
  SDL.raiseWindow window
  say "Engine" "Entering the main loop"
  let draw = do
        drawFrame
          device
          swapchain
          gfxQueue
          presentQueue
          imageAvailable
          renderFinished
          inFlight
          commandBuffers
        Vk.deviceWaitIdle device
   in liftIO $ mainLoop draw

say :: (MonadIO io) => String -> String -> io ()
say prefix msg = liftIO . putStrLn $ prefix ++ ": " ++ msg

sayErr :: (MonadIO io) => String -> String -> io a
sayErr prefix msg = liftIO . throwError . userError $ prefix ++ ": " ++ msg

vulkanVersion :: Word32
vulkanVersion = Vk.API_VERSION_1_3

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
          .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT,
      Vk.messageType =
        Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
      VkDebugUtilsMessengerCreateInfoEXT.pfnUserCallback = Vk.debugCallbackPtr
    }

withBuffer ::
  (Storable a) =>
  Vma.Allocator ->
  Vk.DeviceSize ->
  Vk.Device ->
  Vk.CommandPool ->
  Vk.Queue ->
  Vk.BufferUsageFlagBits ->
  SV.Vector a ->
  Managed Vk.Buffer
withBuffer allocator size device pool queue flags vertices = do
  (staging, ptr) <- withHostBuffer allocator size
  let (src, len) = SV.unsafeToForeignPtr0 vertices
  liftIO . withForeignPtr src $ \s -> copyArray (castPtr ptr) s len

  gpuBuffer <- withGPUBuffer allocator size flags
  copyBuffer device pool queue staging gpuBuffer size
  say "Engine" "Copied staging buffer into GPU buffer"

  return gpuBuffer

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

texture :: Vma.Allocator -> Vk.Device -> Vk.CommandPool -> Vk.Queue -> FilePath -> Managed Vk.ImageView
texture allocator device pool queue path = do
  JP.ImageRGBA8 (JP.Image width height pixels) <- liftIO $ JP.readPng path >>= either (sayErr "Texture" . show) return
  let size = width * height * 4
  (staging, mem) <- withHostBuffer allocator (fromIntegral size)
  liftIO $ copy pixels mem size
  image <- withImage allocator width height
  copyBufferToImage device pool queue staging image width height
  withImageView device image Vk.FORMAT_R8G8B8A8_SRGB
  where
    copy pixels mem size =
      let (src, _) = SV.unsafeToForeignPtr0 pixels
          dst = castPtr mem
       in withForeignPtr src $ \src -> copyArray dst src size

submitNow :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> (Vk.CommandBuffer -> Managed r) -> Managed ()
submitNow device pool queue use = do
  buffer <-
    let info =
          Vk.zero
            { VkCommandBufferAllocateInfo.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
              VkCommandBufferAllocateInfo.commandPool = pool,
              VkCommandBufferAllocateInfo.commandBufferCount = 1
            }
     in V.head <$> managed (Vk.withCommandBuffers device info bracket)
  _ <-
    let info = Vk.zero {VkCommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
     in Vk.useCommandBuffer buffer info $ use buffer
  let info = Vk.SomeStruct $ Vk.zero {VkSubmitInfo.commandBuffers = [Vk.commandBufferHandle buffer]}
   in Vk.queueSubmit queue [info] Vk.NULL_HANDLE
  Vk.queueWaitIdle queue

copyBufferToImage ::
  VkDevice.Device ->
  Vk.CommandPool ->
  Vk.Queue ->
  Vk.Buffer ->
  Vk.Image ->
  Int ->
  Int ->
  Managed ()
copyBufferToImage device pool queue src dst width height = do
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
    tranistFromUndefinedToDstOptimal dst cmd
    Vk.cmdCopyBufferToImage cmd src dst Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL [region]
    tranistFromDstOptimalToShaderReadOnlyOptimal dst cmd

tranistFromUndefinedToDstOptimal :: Vk.Image -> Vk.CommandBuffer -> Managed ()
tranistFromUndefinedToDstOptimal dst cmd =
  transitImage
    cmd
    dst
    (Vk.AccessFlagBits 0)
    Vk.ACCESS_TRANSFER_WRITE_BIT
    Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
    Vk.PIPELINE_STAGE_TRANSFER_BIT

tranistFromDstOptimalToShaderReadOnlyOptimal :: Vk.Image -> Vk.CommandBuffer -> Managed ()
tranistFromDstOptimalToShaderReadOnlyOptimal dst cmd =
  transitImage
    cmd
    dst
    Vk.ACCESS_TRANSFER_WRITE_BIT
    Vk.ACCESS_SHADER_READ_BIT
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT

copyBuffer :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> Vk.Buffer -> Vk.Buffer -> Vk.DeviceSize -> Managed ()
copyBuffer device pool queue src dst size = do
  buffer <-
    let info =
          Vk.zero
            { VkCommandBufferAllocateInfo.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
              VkCommandBufferAllocateInfo.commandPool = pool,
              VkCommandBufferAllocateInfo.commandBufferCount = 1
            }
     in V.head <$> managed (Vk.withCommandBuffers device info bracket)
  let info = Vk.zero {VkCommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
      copy = Vk.zero {VkBufferCopy.srcOffset = 0, VkBufferCopy.dstOffset = 0, VkBufferCopy.size = size}
   in Vk.useCommandBuffer buffer info $ Vk.cmdCopyBuffer buffer src dst [copy]
  let info = Vk.zero {VkSubmitInfo.commandBuffers = [Vk.commandBufferHandle buffer]}
   in Vk.queueSubmit queue [Vk.SomeStruct info] Vk.NULL_HANDLE
  Vk.queueWaitIdle queue

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

mainLoop :: IO () -> IO ()
mainLoop draw = whileM $ do
  quit <- any isQuitEvent <$> waitForEvents
  if quit
    then pure False
    else do
      draw
      pure True
  where
    waitForEvents = (:) <$> SDL.waitEvent <*> SDL.pollEvents

isQuitEvent :: SDL.Event -> Bool
isQuitEvent = \case
  (SDL.Event _ SDL.QuitEvent) -> True
  SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released False (SDL.Keysym _ code _)))
    | code == SDL.KeycodeQ || code == SDL.KeycodeEscape ->
        True
  _ -> False

drawFrame ::
  Vk.Device ->
  Vk.SwapchainKHR ->
  Vk.Queue ->
  Vk.Queue ->
  Vk.Semaphore ->
  Vk.Semaphore ->
  Vk.Fence ->
  V.Vector Vk.CommandBuffer ->
  IO ()
drawFrame dev swapchain gfx present imageAvailable renderFinished inFlight commandBuffers =
  do
    _ <- Vk.waitForFences dev [inFlight] True maxBound
    Vk.resetFences dev [inFlight]
    (_, imageIndex) <-
      Vk.acquireNextImageKHR
        dev
        swapchain
        maxBound
        imageAvailable
        Vk.zero
    let wait = [imageAvailable]
        signal = [renderFinished]
        handle = Vk.commandBufferHandle $ commandBuffers ! fromIntegral imageIndex
        info =
          Vk.zero
            { VkSubmitInfo.waitSemaphores = wait,
              VkSubmitInfo.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT],
              VkSubmitInfo.commandBuffers = [handle],
              VkSubmitInfo.signalSemaphores = signal
            }
     in Vk.queueSubmit gfx [Vk.SomeStruct info] inFlight
    _ <-
      let wait = [renderFinished]
          info =
            Vk.zero
              { VkPresentInfoKHR.waitSemaphores = wait,
                VkPresentInfoKHR.swapchains = [swapchain],
                VkPresentInfoKHR.imageIndices = [imageIndex]
              }
       in Vk.queuePresentKHR present info
    return ()

render ::
  Vk.Pipeline ->
  Vk.PipelineLayout ->
  Vk.DescriptorSet ->
  Vk.Buffer ->
  Vk.Buffer ->
  Vk.Extent2D ->
  (Vk.Image, Vk.ImageView) ->
  Vk.CommandBuffer ->
  Managed ()
render pipeline layout set vertex index extent (img, view) cmd =
  Vk.useCommandBuffer cmd Vk.zero $ do
    transit renderLayout
    Vk.cmdBindPipeline cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
    Vk.cmdBindVertexBuffers cmd 0 [vertex] [0]
    Vk.cmdBindIndexBuffer cmd index 0 Vk.INDEX_TYPE_UINT32
    Vk.cmdBindDescriptorSets cmd Vk.PIPELINE_BIND_POINT_GRAPHICS layout 0 [set] []
    draw
    transit presentLayout
  where
    renderLayout =
      ( Vk.AccessFlagBits 0,
        Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        Vk.IMAGE_LAYOUT_UNDEFINED,
        Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT,
        Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      )
    presentLayout =
      ( Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        Vk.AccessFlagBits 0,
        Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR,
        Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
      )

    transit (srcMask, dstMask, old, new, src, dst) =
      let barrier =
            Vk.zero
              { VkImageMemoryBarrier.srcAccessMask = srcMask,
                VkImageMemoryBarrier.dstAccessMask = dstMask,
                VkImageMemoryBarrier.oldLayout = old,
                VkImageMemoryBarrier.newLayout = new,
                VkImageMemoryBarrier.image = img,
                VkImageMemoryBarrier.subresourceRange =
                  Vk.zero
                    { VkImageSubresourceRange.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                      VkImageSubresourceRange.baseMipLevel = 0,
                      VkImageSubresourceRange.levelCount = 1,
                      VkImageSubresourceRange.baseArrayLayer = 0,
                      VkImageSubresourceRange.layerCount = 1
                    }
              }
       in Vk.cmdPipelineBarrier cmd src dst (Vk.DependencyFlagBits 0) [] [] [Vk.SomeStruct barrier]

    draw =
      let clear = Vk.Color (Vk.Float32 0.0 0.0 0.0 0)
          attachment =
            Vk.zero
              { VkRenderingAttachmentInfo.imageView = view,
                VkRenderingAttachmentInfo.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                VkRenderingAttachmentInfo.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
                VkRenderingAttachmentInfo.storeOp = Vk.ATTACHMENT_STORE_OP_STORE,
                VkRenderingAttachmentInfo.clearValue = clear
              }
          scissor = Vk.Rect2D {VkRect2D.offset = Vk.Offset2D 0 0, VkRect2D.extent = extent}
          info2 =
            Vk.zero
              { VkRenderingInfo.renderArea = scissor,
                VkRenderingInfo.layerCount = 1,
                VkRenderingInfo.colorAttachments = [attachment]
              }
          indexCount = 6 -- TODO get from index buffer
       in Vk.cmdUseRendering cmd info2 $ Vk.cmdDrawIndexed cmd indexCount 1 0 0 0

transitImage ::
  Vk.CommandBuffer ->
  Vk.Image ->
  Vk.AccessFlagBits ->
  Vk.AccessFlagBits ->
  Vk.ImageLayout ->
  Vk.ImageLayout ->
  Vk.PipelineStageFlags ->
  Vk.PipelineStageFlags ->
  Managed ()
transitImage cmd img srcMask dstMask old new srcStage dstStage =
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
  VkExtent2D.Extent2D ->
  V.Vector (Vk.Image, Vk.ImageView) ->
  Vk.Buffer ->
  Vk.Buffer ->
  Vk.ImageView ->
  Vk.Sampler ->
  Managed (V.Vector Vk.CommandBuffer)
createCommandBuffers device pool extent images vertex index texture sampler = do
  (pipeline, layout, set) <- withPipeline device extent texture sampler
  say "Vulkan" "Created pipeline"
  commandBuffers <-
    let info =
          Vk.zero
            { Vk.commandPool = pool,
              Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
              Vk.commandBufferCount = fromIntegral (V.length images)
            }
     in managed $ Vk.withCommandBuffers device info bracket
  say "Vulkan" "Created comamand buffers"
  for_ (V.zip images commandBuffers) $ uncurry (render pipeline layout set vertex index extent)
  say "Vulkan" "Recorded command buffers"
  return commandBuffers

withPipeline ::
  Vk.Device ->
  Vk.Extent2D ->
  Vk.ImageView ->
  Vk.Sampler ->
  Managed (Vk.Pipeline, Vk.PipelineLayout, Vk.DescriptorSet)
withPipeline dev extent texture sampler = do
  (vert, frag) <- createShaders dev
  let descriptorCount = 1
      types =
        [ --Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
          --Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER,
          Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        ]
      binding i typ =
        Vk.zero
          { VkDescriptorSetLayoutBinding.binding = i,
            VkDescriptorSetLayoutBinding.descriptorCount = descriptorCount,
            VkDescriptorSetLayoutBinding.descriptorType = typ,
            VkDescriptorSetLayoutBinding.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
          }
      poolSize typ =
        Vk.zero
          { VkDescriptorPoolSize.descriptorCount = descriptorCount,
            VkDescriptorPoolSize.type' = typ
          }
  setLayout <-
    let info =
          Vk.zero {VkDescriptorSetLayoutCreateInfo.bindings = V.fromList $ zipWith binding [0 ..] types}
     in managed $ Vk.withDescriptorSetLayout dev info Nothing bracket
  pool <-
    let uniform =
          Vk.zero
            { VkDescriptorPoolSize.descriptorCount = 1,
              VkDescriptorPoolSize.type' = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
            }
        sampler =
          Vk.zero
            { VkDescriptorPoolSize.descriptorCount = 1,
              VkDescriptorPoolSize.type' = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            }
        info =
          Vk.zero
            { VkDescriptorPoolCreateInfo.poolSizes = [uniform, sampler],
              VkDescriptorPoolCreateInfo.maxSets = 1,
              VkDescriptorPoolCreateInfo.flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
            }
     in managed $ Vk.withDescriptorPool dev info Nothing bracket
  set <-
    let info =
          Vk.zero
            { VkDescriptorSetAllocateInfo.descriptorPool = pool,
              VkDescriptorSetAllocateInfo.setLayouts = [setLayout]
            }
     in V.head <$> managed (Vk.withDescriptorSets dev info bracket)
  let bufferInfo =
        Vk.zero
          { -- VkDescriptorBufferInfo.buffer = _,
            VkDescriptorBufferInfo.offset = 0,
            VkDescriptorBufferInfo.range = Vk.WHOLE_SIZE
          }
      bufferWriteInfo =
        Vk.zero
          { VkWriteDescriptorSet.dstSet = set,
            VkWriteDescriptorSet.dstBinding = 0,
            VkWriteDescriptorSet.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            VkWriteDescriptorSet.bufferInfo = [bufferInfo]
          }
      imageInfo =
        Vk.zero
          { VkDescriptorImageInfo.imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
            VkDescriptorImageInfo.imageView = texture,
            VkDescriptorImageInfo.sampler = sampler
          }
      imageWriteInfo =
        Vk.SomeStruct
          Vk.zero
            { VkWriteDescriptorSet.dstSet = set,
              VkWriteDescriptorSet.dstBinding = 0,
              VkWriteDescriptorSet.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
              VkWriteDescriptorSet.descriptorCount = 1,
              VkWriteDescriptorSet.imageInfo = [imageInfo]
            }
   in Vk.updateDescriptorSets dev [imageWriteInfo] []

  pipelineLayout <-
    let info = Vk.zero {VkPipelineLayoutCreateInfo.setLayouts = [setLayout]}
     in managed $ Vk.withPipelineLayout dev info Nothing bracket
  (_, res) <-
    let position =
          Vk.zero -- layout(location = 0) in vec2 inPosition
            { VkVertexInputAttributeDescription.binding = 0,
              VkVertexInputAttributeDescription.location = 0,
              VkVertexInputAttributeDescription.format = Vk.FORMAT_R32G32_SFLOAT,
              VkVertexInputAttributeDescription.offset = 0
            }
        xySize = sizeOf (undefined :: G.Vec2)
        rgbSize = sizeOf (undefined :: G.Vec3)
        color =
          let offset = xySize
           in Vk.zero -- layout(location = 1) in vec3 inColor
                { VkVertexInputAttributeDescription.binding = 0,
                  VkVertexInputAttributeDescription.location = 1,
                  VkVertexInputAttributeDescription.format = Vk.FORMAT_R32G32B32_SFLOAT,
                  VkVertexInputAttributeDescription.offset = fromIntegral offset
                }
        textureCoordinates =
          let offset = xySize + rgbSize
           in Vk.zero -- layout(location = 2) in vec2 inTexCoord
                { VkVertexInputAttributeDescription.binding = 0,
                  VkVertexInputAttributeDescription.location = 2,
                  VkVertexInputAttributeDescription.format = Vk.FORMAT_R32G32_SFLOAT, -- vec2 for textture coordinates
                  VkVertexInputAttributeDescription.offset = fromIntegral offset
                }
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
                  VkPipelineVertexInputStateCreateInfo.vertexAttributeDescriptions = [position, color, textureCoordinates]
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
                        Vk.blendEnable = False
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
  return . (,pipelineLayout,set) . V.head $ res

framesInFlight :: Word32
framesInFlight = 1

-- TODO break into two functions
createShaders ::
  (MonadManaged m) =>
  Vk.Device ->
  m (Vk.SomeStruct Vk.PipelineShaderStageCreateInfo, Vk.SomeStruct Vk.PipelineShaderStageCreateInfo)
createShaders dev =
  do
    fragCode <- liftIO $ BS.readFile "frag.spv"
    vertCode <- liftIO $ BS.readFile "vert.spv"
    fragModule <- managed $ Vk.withShaderModule dev Vk.zero {VkShaderModuleCreateInfo.code = fragCode} Nothing bracket
    -- fragModule <- managed $ Vk.withShadersEXT dev Vk.zero {VkShaderModuleCreateInfo.code = fragCode} Nothing bracket
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
            -- VkImageViewCreateInfo.components =
            --   Vk.zero
            --     { VkComponentMapping.r = Vk.COMPONENT_SWIZZLE_IDENTITY,
            --       VkComponentMapping.g = Vk.COMPONENT_SWIZZLE_IDENTITY,
            --       VkComponentMapping.b = Vk.COMPONENT_SWIZZLE_IDENTITY,
            --       VkComponentMapping.a = Vk.COMPONENT_SWIZZLE_IDENTITY
            --     },
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

withDevice :: (MonadManaged m) => Vk.PhysicalDevice -> Word32 -> Word32 -> Bool -> m Vk.Device
withDevice gpu gfx present portable =
  let exts =
        Vk.KHR_DYNAMIC_RENDERING_EXTENSION_NAME
          : Vk.KHR_SWAPCHAIN_EXTENSION_NAME
          : Vk.KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME
          : ([Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME | portable])
      dynamicRendering = Vk.zero {VkPhysicalDeviceDynamicRenderingFeatures.dynamicRendering = True}
      info =
        Vk.zero
          { VkDeviceCreateInfo.queueCreateInfos = V.fromList $ inf gfx : ([inf present | gfx /= present]),
            VkDeviceCreateInfo.enabledExtensionNames = V.fromList exts
          }
          ::& dynamicRendering
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
  forall m.
  (MonadIO m) =>
  Vk.Instance ->
  Vk.SurfaceKHR ->
  m (Maybe (Vk.PhysicalDevice, Word32, Word32, Bool))
pickGPU vulkan surface = do
  (_, gpus) <- Vk.enumeratePhysicalDevices vulkan
  let good' d = ExceptT $ good d <&> (\case Nothing -> Right (); Just ((g, p), pss) -> Left (d, g, p, pss))
   in findM (\_ gpu -> good' gpu) () gpus
  where
    findM f s0 ta = runExceptT (foldlM f s0 ta) >>= (\r -> return $ case r of Left b -> Just b; Right _ -> Nothing)
    found = ExceptT . return . Left
    continue = ExceptT . return . Right

    good gpu = do
      features <- Vk.getPhysicalDeviceFeatures2 gpu :: m (Vk.PhysicalDeviceFeatures2 '[Vk.PhysicalDeviceDescriptorIndexingFeatures])
      let descriptorIndexingFeatures = fst $ VkPhysicalDeviceFeatures2.next features
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
              Vk.EXT_VALIDATION_FEATURES_EXTENSION_NAME,
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
              [ Vk.VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT,
                Vk.VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT
              ]
              []
            :& ()
  managed $ Vk.withInstance instanceCreateInfo Nothing bracket

withSurface :: (MonadManaged m) => SDL.Window -> Vk.Instance -> m Vk.SurfaceKHR
withSurface w v@(Vk.Instance v' _) =
  managed $
    bracket
      (putStrLn "SDL: Creating Vulkan surface" *> create <* putStrLn "SDL: Vulkan surface created")
      (\s -> Vk.destroySurfaceKHR v s Nothing <* putStrLn "Vulkan: Destroyed surface")
  where
    create = Vk.SurfaceKHR <$> SDL.vkCreateSurface w (castPtr v')

withWindow :: (MonadManaged m) => Int -> Int -> m SDL.Window
withWindow width height =
  managed $
    bracket
      (putStrLn "SDL: Creating window" *> create <* putStrLn "SDL: Window created")
      (\w -> putStrLn "SDL: Window destroyed" *> SDL.destroyWindow w)
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

withSDL :: (MonadManaged m) => m ()
withSDL =
  managed_ $
    bracket_
      (ver *> init <* putStrLn "SDL: Initialized")
      (SDL.quit *> putStrLn "SDL: Quit")
      . withVkLib
  where
    ver = SDL.version >>= (\(v0 :: Int, v1, v2) -> putStrLn $ "SDL: Version " ++ show v0 ++ "." ++ show v1 ++ "." ++ show v2)
    init = SDL.initialize ([SDL.InitEvents, SDL.InitVideo] :: [SDL.InitFlag])
    withVkLib = bracket_ (SDL.vkLoadLibrary Nothing) SDL.vkUnloadLibrary