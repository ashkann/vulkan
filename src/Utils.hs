{-# LANGUAGE DataKinds #-}
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
    repeatingSampler,
    withGPUBuffer,
    withSDL2VulkanWindow,
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
import Measure
import SDL qualified
import SDL.Video.Vulkan qualified as SDL
import Vulkan qualified as Vk
import Vulkan qualified as VkApplicationInfo (ApplicationInfo (..))
import Vulkan qualified as VkBufferCopy (BufferCopy (..))
import Vulkan qualified as VkBufferCreateInfo (BufferCreateInfo (..))
import Vulkan qualified as VkBufferImageCopy (BufferImageCopy (..))
import Vulkan qualified as VkCommandBufferAllocateInfo (CommandBufferAllocateInfo (..))
import Vulkan qualified as VkCommandBufferBeginInfo (CommandBufferBeginInfo (..))
import Vulkan qualified as VkExtent2D (Extent2D (..))
import Vulkan qualified as VkExtent3D (Extent3D (..))
import Vulkan qualified as VkImageCopy (ImageCopy (..))
import Vulkan qualified as VkImageMemoryBarrier (ImageMemoryBarrier (..))
import Vulkan qualified as VkImageSubresourceLayers (ImageSubresourceLayers (..))
import Vulkan qualified as VkImageSubresourceRange (ImageSubresourceRange (..))
import Vulkan qualified as VkInstanceCreateInfo (InstanceCreateInfo (..))
import Vulkan qualified as VkOffset3D (Offset3D (..))
import Vulkan qualified as VkSamplerCreateInfo (SamplerCreateInfo (..))
import Vulkan qualified as VkSubmitInfo (SubmitInfo (..))
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import Vulkan.CStruct.Extends qualified as Vk
import Vulkan.Zero qualified as Vk
import VulkanMemoryAllocator qualified as Vma
import VulkanMemoryAllocator qualified as VmaAllocationCreateInfo (AllocationCreateInfo (..))
import Prelude hiding (init)

say :: (MonadIO io) => String -> String -> io ()
say prefix msg = liftIO . putStrLn $ prefix ++ ": " ++ msg

sayErr :: (MonadIO io) => String -> String -> io a
sayErr prefix msg = liftIO . throwError . userError $ prefix ++ ": " ++ msg

applicationName :: (IsString a) => a
applicationName = "Vulkan 2D Engine"

vulkanVersion :: Word32
vulkanVersion = Vk.API_VERSION_1_2

withSDL2VulkanWindow :: ViewportSize -> Managed (SDL.Window, Vk.Instance, Vk.SurfaceKHR)
withSDL2VulkanWindow s = do
  withSDL
  window <- Utils.withWindow s
  vulkan <- Utils.withVulkan window
  surface <- Utils.withSurface window vulkan
  return (window, vulkan, surface)

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
  let lst = unwords (zipWith (\i s -> show i ++ ":" ++ BS.unpack s ++ " ") [0 :: Int ..] exts)
   in say "Vulkan" $ "Instance extenions: " ++ lst
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
          ::& Vk.ValidationFeaturesEXT
            { Vk.enabledValidationFeatures = [Vk.VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT],
              Vk.disabledValidationFeatures = []
            }
            :& ()
   in managed $ Vk.withInstance info Nothing bracket

withSurface :: SDL.Window -> Vk.Instance -> Managed Vk.SurfaceKHR
withSurface w v@(Vk.Instance v' _) = managed $ bracket create destroy
  where
    destroy s = Vk.destroySurfaceKHR v s Nothing <* say "Vulkan" "Destroyed surface"
    create = Vk.SurfaceKHR <$> SDL.vkCreateSurface w (castPtr v') <* say "SDL" "Vulkan surface created"

withWindow :: ViewportSize -> Managed SDL.Window
withWindow (WithVec w h) =
  managed $
    bracket
      (SDL.createWindow applicationName win <* say "SDL" "Created window")
      (\w -> SDL.destroyWindow w <* say "SDL" "Destroyed window")
  where
    win =
      SDL.defaultWindow
        { SDL.windowInitialSize = size,
          SDL.windowGraphicsContext = SDL.VulkanContext,
          SDL.windowPosition = SDL.Centered,
          SDL.windowBorder = False,
          SDL.windowHighDPI = False
        }
    size = SDL.V2 (fromIntegral w) (fromIntegral h)

withSDL :: Managed ()
withSDL = do
  (v0 :: Int, v1, v2) <- SDL.version
  let ver = "version " ++ show v0 ++ "." ++ show v1 ++ "." ++ show v2
  with_ (SDL.initialize flags <* say' ("Initialized " ++ ver)) (SDL.quit <* say' "Quit")
  with_ (SDL.vkLoadLibrary Nothing <* say' "Loaded Vulkan lib") (SDL.vkUnloadLibrary <* say' "Unloaded Vulkan lib")
  where
    say' = say "SDL"
    with_ acq rel = managed_ $ bracket_ acq rel
    flags = [SDL.InitEvents, SDL.InitVideo] :: [SDL.InitFlag]

isQuitEvent :: SDL.Event -> Bool
isQuitEvent = \case
  (SDL.Event _ SDL.QuitEvent) -> True
  SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released False (SDL.Keysym _ code _)))
    | code == SDL.KeycodeEscape ->
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

-- TODO: DRY with copyToGpu
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