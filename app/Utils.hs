{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
  ( say,
    sayErr,
    withSDL,
    withVulkan,
    withSurface,
    withWindow,
    vulkanVersion,
    isQuitEvent,
    withDebug,
    transitImage,
    transitToPresent,
    transitToRenderTarget,
    transitRenderTargetToCopySrc,
    transitToCopyDst,
    tranistToCopyDst,
    transitToGpuRead,
    copyBufferToImage,
    copyToGpu,
    copyToGpu2,
    copyImageToImage,
  )
where

import Control.Exception (bracket, bracket_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (Managed, managed, managed_)
import Data.Bits ((.|.))
import Data.ByteString.Char8 qualified as BS (pack, unpack)
import Data.String (IsString)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import Data.Word (Word32)
import Foreign (Ptr, Storable, castPtr, copyArray, withForeignPtr)
import Foreign.C (peekCAString)
import Foreign.Storable (Storable (..), sizeOf)
import SDL qualified
import SDL.Video.Vulkan qualified as SDL
import Vulkan qualified as Vk
import Vulkan qualified as VkApplicationInfo (ApplicationInfo (..))
import Vulkan qualified as VkBufferCopy (BufferCopy (..))
import Vulkan qualified as VkBufferImageCopy (BufferImageCopy (..))
import Vulkan qualified as VkCommandBufferAllocateInfo (CommandBufferAllocateInfo (..))
import Vulkan qualified as VkCommandBufferBeginInfo (CommandBufferBeginInfo (..))
import Vulkan qualified as VkDebugUtilsMessengerCreateInfoEXT (DebugUtilsMessengerCreateInfoEXT (..))
import Vulkan qualified as VkExtent2D (Extent2D (..))
import Vulkan qualified as VkExtent3D (Extent3D (..))
import Vulkan qualified as VkImageCopy (ImageCopy (..))
import Vulkan qualified as VkImageMemoryBarrier (ImageMemoryBarrier (..))
import Vulkan qualified as VkImageSubresourceLayers (ImageSubresourceLayers (..))
import Vulkan qualified as VkImageSubresourceRange (ImageSubresourceRange (..))
import Vulkan qualified as VkInstanceCreateInfo (InstanceCreateInfo (..))
import Vulkan qualified as VkOffset3D (Offset3D (..))
import Vulkan qualified as VkSubmitInfo (SubmitInfo (..))
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import Vulkan.CStruct.Extends qualified as Vk
import Vulkan.Utils.Debug qualified as Vk
import Vulkan.Zero qualified as Vk

say :: (MonadIO io) => String -> String -> io ()
say prefix msg = liftIO . putStrLn $ prefix ++ ": " ++ msg

sayErr :: (MonadIO io) => String -> String -> io a
sayErr prefix msg = liftIO . throwError . userError $ prefix ++ ": " ++ msg

applicationName :: (IsString a) => a
applicationName = "Vulkan 2D Engine"

vulkanVersion :: Word32
vulkanVersion = Vk.API_VERSION_1_2

withVulkan :: SDL.Window -> Managed Vk.Instance
withVulkan w = do
  exts <-
    let extraExts =
          [ Vk.EXT_DEBUG_UTILS_EXTENSION_NAME,
            "VK_EXT_layer_settings",
            Vk.KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
          ]
        sdlExts = mapM (fmap BS.pack . peekCAString) =<< SDL.vkGetInstanceExtensions w
     in liftIO $ (++ extraExts) <$> sdlExts
  say "Vulkan" $ "Instance extenions: " ++ unwords (BS.unpack <$> exts)
  let info =
        Vk.zero
          { VkInstanceCreateInfo.applicationInfo =
              Just
                Vk.zero
                  { VkApplicationInfo.applicationName = Just applicationName,
                    VkApplicationInfo.apiVersion = vulkanVersion
                  },
            VkInstanceCreateInfo.enabledExtensionNames = V.fromList exts,
            VkInstanceCreateInfo.enabledLayerNames = ["VK_LAYER_KHRONOS_validation"],
            VkInstanceCreateInfo.flags = Vk.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
          }
          ::& debugUtilsMessengerCreateInfo
            :& Vk.ValidationFeaturesEXT
              [ Vk.VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT
                -- Vk.VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT
                -- Vk.VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT
              ]
              []
            :& ()
   in managed $ Vk.withInstance info Nothing bracket

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

withDebug :: Vk.Instance -> Managed ()
withDebug vulkan = do
  _ <- managed $ Vk.withDebugUtilsMessengerEXT vulkan debugUtilsMessengerCreateInfo Nothing bracket
  Vk.submitDebugUtilsMessageEXT
    vulkan
    Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
    Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
    Vk.zero {Vk.message = "Debug Message Test"}

withSurface :: SDL.Window -> Vk.Instance -> Managed Vk.SurfaceKHR
withSurface w v@(Vk.Instance v' _) = managed $ bracket create destroy
  where
    destroy s = Vk.destroySurfaceKHR v s Nothing <* say "Vulkan" "Destroyed surface"
    create = Vk.SurfaceKHR <$> SDL.vkCreateSurface w (castPtr v') <* say "SDL" "Vulkan surface created"

withWindow :: Int -> Int -> Managed SDL.Window
withWindow width height =
  managed $
    bracket
      (SDL.createWindow applicationName win <* say "SDL" "Window created")
      (\w -> SDL.destroyWindow w <* say "SDL" "Window destroyed")
  where
    size = SDL.V2 (fromIntegral width) (fromIntegral height)
    win =
      SDL.defaultWindow
        { SDL.windowInitialSize = size,
          SDL.windowGraphicsContext = SDL.VulkanContext,
          SDL.windowPosition = SDL.Centered
        }

withSDL :: Managed ()
withSDL = do
  liftIO printVersion
  with_ (SDL.initialize flags <* say "SDL" "Initialized") (SDL.quit <* say "SDL" "Quit")
  with_ (SDL.vkLoadLibrary Nothing <* say "SDL" "Loaded Vulkan lib") (SDL.vkUnloadLibrary <* say "SDL" "Unloaded Vulkan lib")
  where
    with_ acq rel = managed_ $ bracket_ acq rel
    printVersion = SDL.version >>= (\(v0 :: Int, v1, v2) -> putStrLn $ "SDL: Version " ++ show v0 ++ "." ++ show v1 ++ "." ++ show v2)
    flags = [SDL.InitEvents :: SDL.InitFlag, SDL.InitVideo] :: [SDL.InitFlag]

isQuitEvent :: SDL.Event -> Bool
isQuitEvent = \case
  (SDL.Event _ SDL.QuitEvent) -> True
  SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released False (SDL.Keysym _ code _)))
    | code == SDL.KeycodeQ || code == SDL.KeycodeEscape ->
        True
  _ -> False

transitRenderTargetToCopySrc :: (MonadIO io) => Vk.CommandBuffer -> Vk.Image -> io ()
transitRenderTargetToCopySrc cmd image =
  let from =
        ( Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
          Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
          Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        )
      to =
        ( Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
          Vk.ACCESS_TRANSFER_READ_BIT,
          Vk.PIPELINE_STAGE_TRANSFER_BIT
        )
   in transitImage
        cmd
        image
        from
        to

transitToCopyDst :: (MonadIO io) => Vk.CommandBuffer -> Vk.Image -> io ()
transitToCopyDst cmd image =
  let from =
        ( Vk.IMAGE_LAYOUT_UNDEFINED,
          Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
          Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        )
      to =
        ( Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
          Vk.ACCESS_TRANSFER_WRITE_BIT,
          Vk.PIPELINE_STAGE_TRANSFER_BIT
        )
   in transitImage
        cmd
        image
        from
        to

transitToPresent :: (MonadIO io) => Vk.CommandBuffer -> Vk.Image -> io ()
transitToPresent cmd image =
  let from =
        ( Vk.IMAGE_LAYOUT_UNDEFINED,
          Vk.ACCESS_TRANSFER_WRITE_BIT,
          Vk.PIPELINE_STAGE_TRANSFER_BIT
        )
      to =
        ( Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR,
          Vk.AccessFlagBits 0,
          Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
        )
   in transitImage
        cmd
        image
        from
        to

transitToRenderTarget :: (MonadIO io) => Vk.CommandBuffer -> Vk.Image -> io ()
transitToRenderTarget cmd image =
  let from =
        ( Vk.IMAGE_LAYOUT_UNDEFINED,
          Vk.AccessFlagBits 0,
          Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
        )
      to =
        ( Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
          Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
          Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        )
   in transitImage
        cmd
        image
        from
        to

tranistToCopyDst :: (MonadIO io) => Vk.CommandBuffer -> Vk.Image -> io ()
tranistToCopyDst cmd image =
  let from =
        ( Vk.IMAGE_LAYOUT_UNDEFINED,
          Vk.AccessFlagBits 0,
          Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
        )
      to =
        ( Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
          Vk.ACCESS_TRANSFER_WRITE_BIT,
          Vk.PIPELINE_STAGE_TRANSFER_BIT
        )
   in transitImage
        cmd
        image
        from
        to

transitToGpuRead :: (MonadIO io) => Vk.CommandBuffer -> Vk.Image -> io ()
transitToGpuRead cmd image =
  let from =
        ( Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
          Vk.ACCESS_TRANSFER_WRITE_BIT,
          Vk.PIPELINE_STAGE_TRANSFER_BIT
        )
      to =
        ( Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
          Vk.ACCESS_SHADER_READ_BIT,
          Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
        )
   in transitImage
        cmd
        image
        from
        to

transitImage ::
  (MonadIO io) =>
  Vk.CommandBuffer ->
  Vk.Image ->
  (Vk.ImageLayout, Vk.AccessFlagBits, Vk.PipelineStageFlags) ->
  (Vk.ImageLayout, Vk.AccessFlagBits, Vk.PipelineStageFlags) ->
  io ()
transitImage cmd img (oldLayout, srcMask, srcStage) (newLayout, dstMask, dstStage) =
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
            VkImageMemoryBarrier.oldLayout = oldLayout,
            VkImageMemoryBarrier.newLayout = newLayout,
            VkImageMemoryBarrier.image = img,
            VkImageMemoryBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkImageMemoryBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkImageMemoryBarrier.subresourceRange = range
          }
   in Vk.cmdPipelineBarrier cmd srcStage dstStage (Vk.DependencyFlagBits 0) [] [] [Vk.SomeStruct barrier]

copyBufferToImage ::
  (MonadIO io) =>
  Vk.Device ->
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
    tranistToCopyDst cmd dst
    Vk.cmdCopyBufferToImage cmd src dst Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL [region]
    transitToGpuRead cmd dst

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

-- TODO DRY with copyToGpu
copyToGpu2 ::
  (Storable a) =>
  Vk.Device ->
  Vk.CommandPool ->
  Vk.Queue ->
  Vk.Buffer ->
  (Vk.Buffer, Ptr ()) ->
  a ->
  IO ()
copyToGpu2 device pool queue gpuBuffer (hostBuffer, hostBufferPtr) a = do
  poke (castPtr hostBufferPtr) a
  let copy cmd = Vk.cmdCopyBuffer cmd hostBuffer gpuBuffer [Vk.zero {VkBufferCopy.size = fromIntegral $ sizeOf a}]
   in submitWait device pool queue copy

copyImageToImage :: (MonadIO io) => Vk.CommandBuffer -> Vk.Image -> Vk.Image -> Vk.Extent2D -> io ()
copyImageToImage cmd source destination extent =
  let subresource =
        Vk.zero
          { VkImageSubresourceLayers.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
            VkImageSubresourceLayers.layerCount = 1,
            VkImageSubresourceLayers.mipLevel = 0,
            VkImageSubresourceLayers.baseArrayLayer = 0
          }
      dims =
        Vk.Extent3D
          { VkExtent3D.width = VkExtent2D.width extent,
            VkExtent3D.height = VkExtent2D.height extent,
            VkExtent3D.depth = 1
          }
      offset0 = Vk.zero {VkOffset3D.x = 0, VkOffset3D.y = 0, VkOffset3D.z = 0}
      region =
        Vk.zero
          { VkImageCopy.srcOffset = offset0,
            VkImageCopy.dstOffset = offset0,
            VkImageCopy.dstSubresource = subresource,
            VkImageCopy.srcSubresource = subresource,
            VkImageCopy.extent = dims
          }
      srcLayout = Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
      dstLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
   in Vk.cmdCopyImage cmd source srcLayout destination dstLayout [region]

-- import DearImGui qualified as ImGui
-- import DearImGui.SDL.Vulkan qualified as ImGui
-- import DearImGui.Vulkan qualified as ImGui
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

-- copyImageToImage2 :: Vk.CommandBuffer -> Vk.Image -> Vk.Image -> Vk.Extent2D -> Vk.Extent2D -> IO ()
-- copyImageToImage2 cmd source destination srcSize dstSize = _

-- Vk.ImageBlit2 blitRegion{ .sType = VK_STRUCTURE_TYPE_IMAGE_BLIT_2, .pNext = nullptr };

-- blitRegion.srcOffsets[1].x = srcSize.width;
-- blitRegion.srcOffsets[1].y = srcSize.height;
-- blitRegion.srcOffsets[1].z = 1;

-- blitRegion.dstOffsets[1].x = dstSize.width;
-- blitRegion.dstOffsets[1].y = dstSize.height;
-- blitRegion.dstOffsets[1].z = 1;

-- blitRegion.srcSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
-- blitRegion.srcSubresource.baseArrayLayer = 0;
-- blitRegion.srcSubresource.layerCount = 1;
-- blitRegion.srcSubresource.mipLevel = 0;

-- blitRegion.dstSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
-- blitRegion.dstSubresource.baseArrayLayer = 0;
-- blitRegion.dstSubresource.layerCount = 1;
-- blitRegion.dstSubresource.mipLevel = 0;

-- VkBlitImageInfo2 blitInfo{ .sType = VK_STRUCTURE_TYPE_BLIT_IMAGE_INFO_2, .pNext = nullptr };
-- blitInfo.dstImage = destination;
-- blitInfo.dstImageLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
-- blitInfo.srcImage = source;
-- blitInfo.srcImageLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
-- blitInfo.filter = VK_FILTER_LINEAR;
-- blitInfo.regionCount = 1;
-- blitInfo.pRegions = &blitRegion;

-- vkCmdBlitImage2(cmd, &blitInfo);

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