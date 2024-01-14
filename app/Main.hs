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

import Control.Applicative ((<|>))
import Control.Exception (bracket, bracket_)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.Extra (whileM)
import Control.Monad.Managed (Managed, MonadIO (liftIO), MonadManaged, managed, managed_, runManaged)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.Bits (testBit, (.&.), (.|.))
import Data.ByteString qualified as BS (readFile)
import Data.ByteString.Char8 qualified as BS (pack, unpack)
import Data.Foldable
import Data.Functor ((<&>))
import Data.Traversable (for)
import Data.Vector ((!))
import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import Foreign (Bits (zeroBits), Storable, Word32, castPtr, copyArray, withForeignPtr)
import Foreign.C (peekCAString, peekCString)
import Foreign.Storable (sizeOf)
import SDL qualified
import SDL.Video.Vulkan qualified as SDL
import Vulkan qualified as Vk
import Vulkan qualified as VkApplicationInfo (ApplicationInfo (..))
import Vulkan qualified as VkAttachmentDescription (AttachmentDescription (..))
import Vulkan qualified as VkAttachmentReference (AttachmentReference (..))
import Vulkan qualified as VkBufferCopy (BufferCopy (..))
import Vulkan qualified as VkBufferCreateInfo (BufferCreateInfo (..))
import Vulkan qualified as VkCommandBufferAllocateInfo (CommandBufferAllocateInfo (..))
import Vulkan qualified as VkCommandBufferBeginInfo (CommandBufferBeginInfo (..))
import Vulkan qualified as VkCommandPoolCreateInfo (CommandPoolCreateInfo (..))
import Vulkan qualified as VkComponentMapping (ComponentMapping (..))
import Vulkan qualified as VkDebugUtilsMessengerCreateInfoEXT (DebugUtilsMessengerCreateInfoEXT (..))
import Vulkan qualified as VkDeviceCreateInfo (DeviceCreateInfo (..))
import Vulkan qualified as VkDeviceQueueCreateInfo (DeviceQueueCreateInfo (..))
import Vulkan qualified as VkExtent2D (Extent2D (..))
import Vulkan qualified as VkFenceCreateInfo (FenceCreateInfo (..))
import Vulkan qualified as VkFramebufferCreateInfo (FramebufferCreateInfo (..))
import Vulkan qualified as VkGraphicsPipelineCreateInfo (GraphicsPipelineCreateInfo (..))
import Vulkan qualified as VkImageSubresourceRange (ImageSubresourceRange (..))
import Vulkan qualified as VkImageViewCreateInfo (ImageViewCreateInfo (..))
import Vulkan qualified as VkInstanceCreateInfo (InstanceCreateInfo (..))
import Vulkan qualified as VkMemoryAllocateInfo (MemoryAllocateInfo (..))
import Vulkan qualified as VkMemoryRequirements (MemoryRequirements (..))
import Vulkan qualified as VkMemoryType (MemoryType (..))
import Vulkan qualified as VkPipelineColorBlendStateCreateInfo (PipelineColorBlendStateCreateInfo (..))
import Vulkan qualified as VkPipelineShaderStageCreateInfo (PipelineShaderStageCreateInfo (..))
import Vulkan qualified as VkPipelineVertexInputStateCreateInfo (PipelineVertexInputStateCreateInfo (..))
import Vulkan qualified as VkPresentInfoKHR (PresentInfoKHR (..))
import Vulkan qualified as VkRect2D (Rect2D (..))
import Vulkan qualified as VkRenderPassBeginInfo (RenderPassBeginInfo (..))
import Vulkan qualified as VkRenderPassCreateInfo (RenderPassCreateInfo (..))
import Vulkan qualified as VkShaderModuleCreateInfo (ShaderModuleCreateInfo (..))
import Vulkan qualified as VkSubmitInfo (SubmitInfo (..))
import Vulkan qualified as VkSubpassDependency (SubpassDependency (..))
import Vulkan qualified as VkSubpassDescription (SubpassDescription (..))
import Vulkan qualified as VkSurface (SurfaceFormatKHR (..))
import Vulkan qualified as VkSurfaceCaps (SurfaceCapabilitiesKHR (..))
import Vulkan qualified as VkSwapchainCreateInfo (SwapchainCreateInfoKHR (..))
import Vulkan qualified as VkVPipelineMultisampleStateCreateInfo (PipelineMultisampleStateCreateInfo (..))
import Vulkan qualified as VkVertexInputAttributeDescription (VertexInputAttributeDescription (..))
import Vulkan qualified as VkVertexInputBindingDescription (VertexInputBindingDescription (..))
import Vulkan qualified as VkViewport (Viewport (..))
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import Vulkan.CStruct.Extends qualified as Vk
import Vulkan.Utils.Debug qualified as Vk
import Vulkan.Zero qualified as Vk
import Prelude hiding (init)

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
  say "Vulkan" "Device created"
  gfxQueue <- Vk.getDeviceQueue device gfx 0
  say "Vulkan" "Got graphics queue"
  presentQueue <- Vk.getDeviceQueue device present 0
  say "Vulkan" "Got present queue"
  say "Vulkan" "Creating swap chain"
  (swapchain, imageViews, extent) <-
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
  vertexBuffer <-
    let vertices =
          SV.fromList
            [ -- v0
              0.5, -- x
              0.5, -- y
              1.0, -- r
              0, -- g
              0, -- b
              -- v1
              -0.5,
              0.5,
              0,
              1.0,
              0,
              -- v2
              -0.5,
              -0.5,
              0,
              0,
              1.0,
              -- v3
              0.5,
              -0.5,
              0.5,
              0.5,
              0.5
            ] ::
            SV.Vector Float
     in runMaybeT (withBuffer2 gpu device commandPool gfxQueue Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT vertices)
          >>= \case
            Just x -> return x
            Nothing -> sayErr "Vulkan" "Failed to create vertex buffer"
  say "Vulkan" "Created vertex buffer"
  indexBuffer <-
    let indecies = SV.fromList [0, 1, 2, 2, 3, 0] :: SV.Vector Word32
     in runMaybeT (withBuffer2 gpu device commandPool gfxQueue Vk.BUFFER_USAGE_INDEX_BUFFER_BIT indecies) >>= \case
          Just x -> return x
          Nothing -> sayErr "Vulkan" "Failed to create index buffer"
  say "Vulkan" "Created index buffer"
  commandBuffers <- createCommandBuffers device commandPool extent imageViews vertexBuffer indexBuffer
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
        Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
      Vk.messageType =
        Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
      VkDebugUtilsMessengerCreateInfoEXT.pfnUserCallback = Vk.debugCallbackPtr
    }

createCommandBuffers ::
  Vk.Device ->
  Vk.CommandPool ->
  VkExtent2D.Extent2D ->
  V.Vector Vk.ImageView ->
  Vk.Buffer ->
  Vk.Buffer ->
  Managed (V.Vector Vk.CommandBuffer)
createCommandBuffers device pool extent imageViews vertexBuffer indexBuffer = do
  renderPass <- createRenderPass device
  say "Vulkan" "Created render pass"
  pipeline <- withPipeline device renderPass extent
  say "Vulkan" "Created pipeline"
  framebuffers <- withFramebuffers renderPass
  say "Vulkan" "Created framebuffers"
  commandBuffers <- withCommandBuffers pool (V.length framebuffers)
  say "Vulkan" "Created comamand buffers"
  _ <- liftIO . for (V.zip framebuffers commandBuffers) $ use renderPass pipeline
  return commandBuffers
  where
    withFramebuffers renderPass =
      for imageViews $ \iv ->
        let inf =
              Vk.zero
                { VkFramebufferCreateInfo.renderPass = renderPass,
                  VkFramebufferCreateInfo.attachments = [iv],
                  VkFramebufferCreateInfo.width = VkExtent2D.width extent,
                  VkFramebufferCreateInfo.height = VkExtent2D.height extent,
                  VkFramebufferCreateInfo.layers = 1
                }
         in managed $ Vk.withFramebuffer device inf Nothing bracket
    withCommandBuffers commandPool count = do
      let info =
            Vk.zero
              { Vk.commandPool = commandPool,
                Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
                Vk.commandBufferCount = fromIntegral count
              }
       in managed $ Vk.withCommandBuffers device info bracket
    use renderPass pipeline (framebuffer, commandBuffer) = Vk.useCommandBuffer commandBuffer info pass
      where
        info = Vk.zero {VkCommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT}
        subpass =
          Vk.zero
            { VkRenderPassBeginInfo.renderPass = renderPass,
              VkRenderPassBeginInfo.framebuffer = framebuffer,
              VkRenderPassBeginInfo.renderArea = Vk.Rect2D {VkRect2D.offset = Vk.zero, VkRect2D.extent = extent},
              VkRenderPassBeginInfo.clearValues = [Vk.Color (Vk.Float32 0.0 0.0 0.0 0)]
            }
        pass =
          Vk.cmdUseRenderPass
            commandBuffer
            subpass
            Vk.SUBPASS_CONTENTS_INLINE
            render
        render = do
          Vk.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
          Vk.cmdBindVertexBuffers commandBuffer 0 [vertexBuffer] [0]
          Vk.cmdBindIndexBuffer commandBuffer indexBuffer 0 Vk.INDEX_TYPE_UINT32
          Vk.cmdDrawIndexed commandBuffer 6 1 0 0 0

withBuffer2 ::
  (Storable a) =>
  Vk.PhysicalDevice ->
  Vk.Device ->
  Vk.CommandPool ->
  Vk.Queue ->
  Vk.BufferUsageFlagBits ->
  SV.Vector a ->
  MaybeT Managed Vk.Buffer
withBuffer2 gpu device pool queue flags vertices = do
  let size = 1024
  (staging, stagingMem) <-
    withBuffer
      gpu
      device
      size
      Vk.BUFFER_USAGE_TRANSFER_SRC_BIT
      (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)

  _ <-
    let action ptr = do
          say "Vulkan" "Statging Buffer memory mapped"
          let (src, len) = SV.unsafeToForeignPtr0 vertices
          liftIO . withForeignPtr src $ \s -> copyArray (castPtr ptr) s len
          say "Engine" "Copied vertices into staging buffer"
     in liftIO $
          Vk.withMappedMemory
            device
            stagingMem
            0
            size
            (Vk.MemoryMapFlags 0)
            bracket
            action

  (buffer, _) <-
    withBuffer
      gpu
      device
      1024
      (Vk.BUFFER_USAGE_TRANSFER_DST_BIT .|. flags)
      Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  lift $ copyBuffer device pool queue staging buffer size
  return buffer

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

withBuffer ::
  Vk.PhysicalDevice ->
  Vk.Device ->
  Vk.DeviceSize ->
  Vk.BufferUsageFlagBits ->
  Vk.MemoryPropertyFlags ->
  MaybeT Managed (Vk.Buffer, Vk.DeviceMemory)
withBuffer gpu device size usage flags = do
  buffer <-
    let info =
          Vk.zero
            { VkBufferCreateInfo.size = size,
              VkBufferCreateInfo.usage = usage,
              VkBufferCreateInfo.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
            }
     in managed $ Vk.withBuffer device info Nothing bracket
  say "Vulkan" $ "Created buffer of size" ++ show size
  req <- Vk.getBufferMemoryRequirements device buffer
  (index, _) <- findMemType req
  mem <- allocMem index $ VkMemoryRequirements.size req
  say "Vulkan" "Memory allocated for buffer"
  Vk.bindBufferMemory device buffer mem 0
  say "Vulkan" "Buffer memory bound"
  return (buffer, mem)
  where
    findMemType req = do
      props <- Vk.getPhysicalDeviceMemoryProperties gpu
      let bits = VkMemoryRequirements.memoryTypeBits req
          good (i, typ) = (VkMemoryType.propertyFlags typ .&. flags == flags) && (bits `testBit` i)
          memTypes = zip [0 ..] (V.toList . Vk.memoryTypes $ props)
      MaybeT . pure $ find good memTypes

    allocMem i s =
      let info =
            Vk.zero
              { VkMemoryAllocateInfo.memoryTypeIndex = fromIntegral i,
                VkMemoryAllocateInfo.allocationSize = s
              }
       in managed $ Vk.withMemory device info Nothing bracket

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

withPipeline :: Vk.Device -> Vk.RenderPass -> Vk.Extent2D -> Managed Vk.Pipeline
withPipeline dev renderPass swapchainExtent = do
  (vert, frag) <- createShaders dev
  pipelineLayout <- managed $ Vk.withPipelineLayout dev Vk.zero Nothing bracket
  let vertexInputInfo =
        Vk.zero
          { VkPipelineVertexInputStateCreateInfo.vertexBindingDescriptions =
              [ Vk.zero
                  { VkVertexInputBindingDescription.binding = 0,
                    VkVertexInputBindingDescription.stride = fromIntegral $ 5 * sizeOf (undefined :: Float),
                    VkVertexInputBindingDescription.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
                  }
              ],
            VkPipelineVertexInputStateCreateInfo.vertexAttributeDescriptions =
              [ Vk.zero
                  { VkVertexInputAttributeDescription.binding = 0,
                    VkVertexInputAttributeDescription.location = 0,
                    VkVertexInputAttributeDescription.format = Vk.FORMAT_R32G32_SFLOAT, -- vec2 for position
                    VkVertexInputAttributeDescription.offset = 0 -- offset of position
                  },
                Vk.zero
                  { VkVertexInputAttributeDescription.binding = 0,
                    VkVertexInputAttributeDescription.location = 1,
                    VkVertexInputAttributeDescription.format = Vk.FORMAT_R32G32B32_SFLOAT,
                    VkVertexInputAttributeDescription.offset = fromIntegral $ 2 * sizeOf (undefined :: Float)
                  }
              ]
          }
      pipelineCreateInfo :: Vk.GraphicsPipelineCreateInfo '[]
      pipelineCreateInfo =
        Vk.zero
          { VkGraphicsPipelineCreateInfo.stages = [vert, frag],
            VkGraphicsPipelineCreateInfo.vertexInputState = Just $ Vk.SomeStruct vertexInputInfo,
            VkGraphicsPipelineCreateInfo.inputAssemblyState =
              Just
                Vk.zero
                  { Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
                    Vk.primitiveRestartEnable = False
                  },
            VkGraphicsPipelineCreateInfo.viewportState =
              Just . Vk.SomeStruct $
                Vk.zero
                  { Vk.viewports =
                      [ Vk.Viewport
                          { VkViewport.x = 0,
                            VkViewport.y = 0,
                            VkViewport.width = realToFrac (VkExtent2D.width swapchainExtent),
                            VkViewport.height = realToFrac (VkExtent2D.height swapchainExtent),
                            VkViewport.minDepth = 0,
                            VkViewport.maxDepth = 1
                          }
                      ],
                    Vk.scissors =
                      [Vk.Rect2D {VkRect2D.offset = Vk.Offset2D 0 0, VkRect2D.extent = swapchainExtent}]
                  },
            VkGraphicsPipelineCreateInfo.rasterizationState =
              Just . Vk.SomeStruct $
                Vk.zero
                  { Vk.depthClampEnable = False,
                    Vk.rasterizerDiscardEnable = False,
                    Vk.lineWidth = 1,
                    Vk.polygonMode = Vk.POLYGON_MODE_FILL,
                    Vk.cullMode = Vk.CULL_MODE_NONE,
                    Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE,
                    Vk.depthBiasEnable = False
                  },
            VkGraphicsPipelineCreateInfo.multisampleState =
              Just . Vk.SomeStruct $
                Vk.zero
                  { VkVPipelineMultisampleStateCreateInfo.sampleShadingEnable = False,
                    VkVPipelineMultisampleStateCreateInfo.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT,
                    VkVPipelineMultisampleStateCreateInfo.minSampleShading = 1,
                    VkVPipelineMultisampleStateCreateInfo.sampleMask = [maxBound]
                  },
            VkGraphicsPipelineCreateInfo.depthStencilState = Nothing,
            VkGraphicsPipelineCreateInfo.colorBlendState =
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
                  },
            VkGraphicsPipelineCreateInfo.dynamicState = Nothing,
            VkGraphicsPipelineCreateInfo.layout = pipelineLayout,
            VkGraphicsPipelineCreateInfo.renderPass = renderPass,
            VkGraphicsPipelineCreateInfo.subpass = 0,
            VkGraphicsPipelineCreateInfo.basePipelineHandle = Vk.zero
          }
  (_, res) <- managed $ Vk.withGraphicsPipelines dev Vk.zero [Vk.SomeStruct pipelineCreateInfo] Nothing bracket
  return . V.head $ res

createShaders ::
  (MonadManaged m) =>
  Vk.Device ->
  m (Vk.SomeStruct Vk.PipelineShaderStageCreateInfo, Vk.SomeStruct Vk.PipelineShaderStageCreateInfo)
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
  (MonadManaged m) =>
  Vk.PhysicalDevice ->
  Vk.Device ->
  Vk.SurfaceKHR ->
  Word32 ->
  Word32 ->
  Int ->
  Int ->
  m (Vk.SwapchainKHR, V.Vector Vk.ImageView, Vk.Extent2D)
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
      let imageViewCreateInfo i =
            Vk.zero
              { VkImageViewCreateInfo.image = i,
                VkImageViewCreateInfo.viewType = Vk.IMAGE_VIEW_TYPE_2D,
                VkImageViewCreateInfo.format = imageFormat,
                VkImageViewCreateInfo.components =
                  Vk.zero
                    { VkComponentMapping.r = Vk.COMPONENT_SWIZZLE_IDENTITY,
                      VkComponentMapping.g = Vk.COMPONENT_SWIZZLE_IDENTITY,
                      VkComponentMapping.b = Vk.COMPONENT_SWIZZLE_IDENTITY,
                      VkComponentMapping.a = Vk.COMPONENT_SWIZZLE_IDENTITY
                    },
                VkImageViewCreateInfo.subresourceRange =
                  Vk.zero
                    { VkImageSubresourceRange.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                      VkImageSubresourceRange.baseMipLevel = 0,
                      VkImageSubresourceRange.levelCount = 1,
                      VkImageSubresourceRange.baseArrayLayer = 0,
                      VkImageSubresourceRange.layerCount = 1
                    }
              }
      viewImages <- for images $ \i -> managed $ Vk.withImageView dev (imageViewCreateInfo i) Nothing bracket
      return (swapchain, viewImages, extent)

createRenderPass :: Vk.Device -> Managed Vk.RenderPass
createRenderPass dev = do
  let attchment :: Vk.AttachmentDescription
      attchment =
        Vk.zero
          { VkAttachmentDescription.format = imageFormat,
            VkAttachmentDescription.samples = Vk.SAMPLE_COUNT_1_BIT,
            VkAttachmentDescription.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
            VkAttachmentDescription.storeOp = Vk.ATTACHMENT_STORE_OP_STORE,
            VkAttachmentDescription.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE,
            VkAttachmentDescription.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE,
            VkAttachmentDescription.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
            VkAttachmentDescription.finalLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
          }
      subpass :: Vk.SubpassDescription
      subpass =
        Vk.zero
          { VkSubpassDescription.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS,
            VkSubpassDescription.colorAttachments =
              [ Vk.zero
                  { VkAttachmentReference.attachment = 0,
                    VkAttachmentReference.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                  }
              ]
          }
      dependency :: Vk.SubpassDependency
      dependency =
        Vk.zero
          { VkSubpassDependency.srcSubpass = Vk.SUBPASS_EXTERNAL,
            VkSubpassDependency.dstSubpass = 0,
            VkSubpassDependency.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            VkSubpassDependency.srcAccessMask = Vk.zero,
            VkSubpassDependency.dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            VkSubpassDependency.dstAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_READ_BIT .|. Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          }
  managed $
    Vk.withRenderPass
      dev
      Vk.zero
        { VkRenderPassCreateInfo.attachments = [attchment],
          VkRenderPassCreateInfo.subpasses = [subpass],
          VkRenderPassCreateInfo.dependencies = [dependency]
        }
      Nothing
      bracket

withDevice :: (MonadManaged m) => Vk.PhysicalDevice -> Word32 -> Word32 -> Bool -> m Vk.Device
withDevice gpu gfx present portable = managed $ Vk.withDevice gpu info Nothing bracket
  where
    inf i =
      Vk.SomeStruct $
        Vk.zero
          { VkDeviceQueueCreateInfo.queueFamilyIndex = i,
            VkDeviceQueueCreateInfo.queuePriorities = [1.0]
          }
    exts = Vk.KHR_SWAPCHAIN_EXTENSION_NAME : ([Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME | portable])
    info =
      Vk.zero
        { VkDeviceCreateInfo.queueCreateInfos = V.fromList $ inf gfx : ([inf present | gfx /= present]),
          VkDeviceCreateInfo.enabledExtensionNames = V.fromList exts
        }

pickGPU ::
  forall m.
  (MonadIO m) =>
  Vk.Instance ->
  Vk.SurfaceKHR ->
  m (Maybe (Vk.PhysicalDevice, Word32, Word32, Bool))
pickGPU vulkan surface = do
  (_, ds) <- Vk.enumeratePhysicalDevices vulkan
  let good' d = ExceptT $ good d <&> (\case Nothing -> Right (); Just ((g, p), pss) -> Left (d, g, p, pss))
   in findM (\_ d -> good' d) () ds
  where
    findM f s0 ta = runExceptT (foldlM f s0 ta) >>= (\r -> return $ case r of Left b -> Just b; Right _ -> Nothing)
    found = ExceptT . return . Left
    continue = ExceptT . return . Right

    good device = do
      (_, exts) <- Vk.enumerateDeviceExtensionProperties device Nothing
      r <- swapchainSupported exts
      if r
        then do
          qs <- Vk.getPhysicalDeviceQueueFamilyProperties device
          maybeQs <- findM ff (Nothing, Nothing) (V.indexed qs)
          return $ (,portabilitySubSetPresent exts) <$> maybeQs
        else return Nothing
      where
        ff (gfx, present) (index, q)
          | Vk.queueCount q <= 0 = continue (gfx, present)
          | otherwise =
              let i = fromIntegral index
                  isGfx = ((Vk.QUEUE_GRAPHICS_BIT .&. Vk.queueFlags q) /= zeroBits) && (Vk.queueCount q > 0)
                  isPresent = Vk.getPhysicalDeviceSurfaceSupportKHR device i surface
                  pick b = if b then Just i else Nothing
                  gfx2 = present <|> pick isGfx
                  present2 = maybe (pick <$> isPresent) (pure . Just) gfx
                  nxt (Just g) (Just p) = found (g, p)
                  nxt g p = continue (g, p)
               in present2 >>= nxt gfx2

        swapchainSupported exts = do
          (_, formats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR device surface
          (_, modes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR device surface
          let isGood f =
                VkSurface.format f == imageFormat
                  && VkSurface.colorSpace f == colorSpace
              swapChain =
                any ((== Vk.KHR_SWAPCHAIN_EXTENSION_NAME) . Vk.extensionName) exts
                  && V.any isGood formats
                  && V.any (== presentMode) modes
          return swapChain

        portabilitySubSetPresent = any ((== Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME) . Vk.extensionName)

withVulkan :: (MonadManaged m) => SDL.Window -> m Vk.Instance
withVulkan w = do
  reqExts <- SDL.vkGetInstanceExtensions w
  liftIO $ do
    exts <- unwords <$> mapM peekCString reqExts
    putStrLn $ "SDL: Required Vulkan extension: " ++ exts
  instExts <- liftIO $ do
    let extra =
          [ Vk.EXT_DEBUG_UTILS_EXTENSION_NAME,
            Vk.EXT_VALIDATION_FEATURES_EXTENSION_NAME,
            "VK_KHR_portability_enumeration",
            "VK_KHR_get_physical_device_properties2"
          ]
    required <- mapM (fmap BS.pack . peekCAString) reqExts
    return $ V.fromList $ required ++ extra
  liftIO $
    let exts = unwords (V.toList (BS.unpack <$> instExts))
     in putStrLn $ "Vulkan: Instance extenions: " ++ exts
  let instanceCreateInfo =
        Vk.zero
          { VkInstanceCreateInfo.applicationInfo =
              Just
                Vk.zero
                  { VkApplicationInfo.applicationName = Just "Vulkan",
                    VkApplicationInfo.apiVersion = Vk.API_VERSION_1_0
                  },
            VkInstanceCreateInfo.enabledExtensionNames = instExts,
            VkInstanceCreateInfo.enabledLayerNames = ["VK_LAYER_KHRONOS_validation"],
            VkInstanceCreateInfo.flags = Vk.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
          }
          ::& debugUtilsMessengerCreateInfo
            :& Vk.ValidationFeaturesEXT [Vk.VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT] []
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