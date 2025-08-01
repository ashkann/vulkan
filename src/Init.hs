{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

module Init
  ( pickGPU,
    presentMode,
    imageFormat,
    colorSpace,
    withSwapChain,
    withDevice,
    QueueFamilyIndex (..),
    withMemoryAllocator,
    withShaders,
  )
where

import Control.Exception (bracket)
import Control.Monad.Except (MonadError (catchError, throwError), forM_)
import Control.Monad.Managed
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Bits (Bits (zeroBits), (.&.), (.|.))
import Data.Coerce (coerce)
import Data.Monoid (Alt (Alt, getAlt))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Ptr (castFunPtr)
import Measure
import Utils
import qualified Vulkan as Ft (PhysicalDeviceVulkan12Features (..))
import qualified Vulkan as Vk
import qualified Vulkan as VkDevice (Device (..))
import qualified Vulkan as VkDeviceCreateInfo (DeviceCreateInfo (..))
import qualified Vulkan as VkDeviceQueueCreateInfo (DeviceQueueCreateInfo (..))
import qualified Vulkan as VkInstance (Instance (..))
import qualified Vulkan as VkPhysicalDeviceDynamicRenderingFeatures (PhysicalDeviceDynamicRenderingFeatures (..))
import qualified Vulkan as VkPhysicalDeviceFeatures2 (PhysicalDeviceFeatures2 (..))
import qualified Vulkan as VkPhysicalDeviceVulkan12Features (PhysicalDeviceVulkan12Features (..))
import qualified Vulkan as VkPipelineShaderStageCreateInfo (PipelineShaderStageCreateInfo (..))
import qualified Vulkan as VkShaderModuleCreateInfo (ShaderModuleCreateInfo (..))
import qualified Vulkan as VkSurfaceCaps (SurfaceCapabilitiesKHR (..))
import qualified Vulkan as VkSurfaceFormat (SurfaceFormatKHR (..))
import qualified Vulkan as VkSwapchainCreateInfo (SwapchainCreateInfoKHR (..))
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Dynamic as Vk
import qualified Vulkan.Zero as Vk
import qualified VulkanMemoryAllocator as Vma
import qualified VulkanMemoryAllocator as VmaAllocatorCreateInfo (AllocatorCreateInfo (..))
import qualified Data.ByteString  as BS (readFile)

presentMode :: Vk.PresentModeKHR
presentMode = Vk.PRESENT_MODE_FIFO_KHR

imageFormat :: Vk.Format
imageFormat = Vk.FORMAT_B8G8R8A8_SRGB

colorSpace :: Vk.ColorSpaceKHR
colorSpace = Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR

-- | Picks up the first suitable GPU
pickGPU ::
  forall io.
  (MonadIO io) =>
  Vk.Instance ->
  Vk.SurfaceKHR ->
  io (Vk.PhysicalDevice, QueueFamilyIndex Graphics, QueueFamilyIndex Present, Bool, String)
pickGPU vulkan surface = do
  (_, gpus) <- Vk.enumeratePhysicalDevices vulkan
  maybeGpu <- runMaybeT . getAlt $ V.foldMap (Alt . isGood) gpus
  maybe (sayErr "Vulkan" "No suitable GPU found") pure maybeGpu
  where
    isGood gpu = do
      name <- unpack . decodeUtf8 . Vk.deviceName <$> Vk.getPhysicalDeviceProperties gpu
      say "Vulkan" $ "Checking GPU \"" ++ name ++ "\" for requirements"
      outcome <- runExceptT $ suitable gpu surface
      (gfx, present, portability) <- either (notGood name) pure outcome
      return (gpu, QueueFamilyIndex gfx, QueueFamilyIndex present, portability, name)
    notGood name es = forM_ (reverse $ "Not suitable" : es) (say name) *> MaybeT (pure Nothing)

-- | Checks if the GPU has the required features
suitable :: (MonadError [String] io, MonadIO io) => Vk.PhysicalDevice -> Vk.SurfaceKHR -> io (Word32, Word32, Bool)
suitable gpu surface = do
  bindlessDescriptors gpu `check` "Bindless descriptors not supported"
  (_, exts) <- Vk.enumerateDeviceExtensionProperties gpu Nothing
  swapchain gpu surface exts `check` "Swapchain checks failed"
  dynamicRendering exts `check` "Dynamic Rendering not supported"
  (gfx, present) <- queues gpu surface `check` "Queue families not found"
  return (gfx, present, needsPortability exts)
  where
    check io msg = io `catchError` (\es -> throwError (msg : es))

-- | GPU supports bindless descriptors
bindlessDescriptors :: forall io. (MonadError [String] io, MonadIO io) => Vk.PhysicalDevice -> io ()
bindlessDescriptors gpu = do
  (ft2, _) <- VkPhysicalDeviceFeatures2.next <$> (Vk.getPhysicalDeviceFeatures2 gpu :: io (Vk.PhysicalDeviceFeatures2 '[Vk.PhysicalDeviceVulkan12Features]))
  Ft.runtimeDescriptorArray ft2 `orError` "\"runtimeDescriptorArray\" is false"
  Ft.descriptorBindingPartiallyBound ft2 `orError` "\"descriptorBindingPartiallyBound\" is false"
  Ft.descriptorBindingSampledImageUpdateAfterBind ft2 `orError` "\"descriptorBindingSampledImageUpdateAfterBind\" is false"

-- | GPU and surface support the swapchain, color space and image format
swapchain ::
  (MonadError [String] io, MonadIO io) =>
  Vk.PhysicalDevice ->
  Vk.SurfaceKHR ->
  V.Vector Vk.ExtensionProperties ->
  io ()
swapchain gpu surface exts = do
  any ((== Vk.KHR_SWAPCHAIN_EXTENSION_NAME) . Vk.extensionName) exts `orError` "Swapchain extension not present"
  (_, formats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR gpu surface
  V.any isGood formats `orError` noGoodSurfaceFormat
  (_, modes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR gpu surface
  V.any (== presentMode) modes `orError` ("Swapchain present mode \"" ++ show presentMode ++ "\"not supported")
  where
    noGoodSurfaceFormat =
      "Swpachain surface format with color format \""
        ++ show imageFormat
        ++ "\" and color space \""
        ++ show colorSpace
        ++ "\" not found"
    isGood f = VkSurfaceFormat.format f == imageFormat && VkSurfaceFormat.colorSpace f == colorSpace

dynamicRendering :: (MonadError [String] m) => V.Vector Vk.ExtensionProperties -> m ()
dynamicRendering props =
  V.any ((== Vk.KHR_DYNAMIC_RENDERING_EXTENSION_NAME) . Vk.extensionName) props
    `orError` "Dynamic Rendering extension not present"

orError :: (MonadError [String] io) => Bool -> String -> io ()
orError False msg = throwError [msg]
orError True _ = pure ()

needsPortability :: V.Vector Vk.ExtensionProperties -> Bool
needsPortability = any ((== Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME) . Vk.extensionName)

-- | State of an ongoing search for queue families
data QueueLookup = NoneFound | GfxFound Word32 | PresentFound Word32

newtype QueuesFound = Queues (Word32, Word32)

-- | GPU should support graphics and present queue families
queues :: forall io. (MonadError [String] io, MonadIO io) => Vk.PhysicalDevice -> Vk.SurfaceKHR -> io (Word32, Word32)
queues gpu surface = do
  qs <- Vk.getPhysicalDeviceQueueFamilyProperties gpu
  out <- runExceptT (V.ifoldM lookup NoneFound qs)
  either pure notGood (coerce out)
  where
    lookup :: QueueLookup -> Int -> Vk.QueueFamilyProperties -> ExceptT QueuesFound io QueueLookup
    lookup acc _ q | Vk.queueCount q <= 0 = pure acc
    lookup acc index q =
      let i = fromIntegral index
          maybeGfx = foundIf $ (Vk.QUEUE_GRAPHICS_BIT .&. Vk.queueFlags q) /= zeroBits
          maybePresent = foundIf <$> Vk.getPhysicalDeviceSurfaceSupportKHR gpu i surface
          foundIf b = if b then Just i else Nothing
          (gfx, ioPresent) =
            case acc of
              GfxFound gfx -> (Just gfx, maybePresent)
              PresentFound present -> (maybeGfx, pure (Just present))
              NoneFound -> (maybeGfx, maybePresent)
       in do
            present <- ioPresent
            case (gfx, present) of
              (Just gfx, Just present) -> throwE $ coerce (gfx, present)
              (Just gfx, Nothing) -> pure $ GfxFound gfx
              (Nothing, Just present) -> pure $ PresentFound present
              (Nothing, Nothing) -> pure NoneFound

    notGood :: QueueLookup -> io (Word32, Word32)
    notGood NoneFound = throwError ["No graphics or present queue families found"]
    notGood (GfxFound _) = throwError ["No graphics queue families found"]
    notGood (PresentFound _) = throwError ["No present queue families found"]

withSwapChain ::
  Vk.PhysicalDevice ->
  Vk.Device ->
  Vk.SurfaceKHR ->
  QueueFamilyIndex Graphics ->
  QueueFamilyIndex Present ->
  ViewportSize ->
  Managed (Vk.SwapchainKHR, Vk.Extent2D, V.Vector Vk.Image)
withSwapChain
  gpu
  dev
  surface
  (QueueFamilyIndex gfx)
  (QueueFamilyIndex present)
  (WithVec width height) =
    do
      caps <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR gpu surface
      let minImageCount = VkSurfaceCaps.minImageCount caps
          transform = VkSurfaceCaps.currentTransform caps
          (sharingMode, queues) =
            if gfx == present
              then (Vk.SHARING_MODE_EXCLUSIVE, [])
              else (Vk.SHARING_MODE_CONCURRENT, V.fromList [gfx, present])
          extent = case VkSurfaceCaps.currentExtent caps of
            Vk.Extent2D w h
              | w == maxBound,
                h == maxBound ->
                  Vk.Extent2D width height
            e -> e
          info =
            Vk.zero
              { VkSwapchainCreateInfo.surface = surface,
                VkSwapchainCreateInfo.minImageCount = minImageCount + 1,
                VkSwapchainCreateInfo.imageFormat = Init.imageFormat,
                VkSwapchainCreateInfo.imageColorSpace = Init.colorSpace,
                VkSwapchainCreateInfo.imageExtent = extent,
                VkSwapchainCreateInfo.imageArrayLayers = 1,
                VkSwapchainCreateInfo.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_TRANSFER_DST_BIT,
                VkSwapchainCreateInfo.imageSharingMode = sharingMode,
                VkSwapchainCreateInfo.queueFamilyIndices = queues,
                VkSwapchainCreateInfo.preTransform = transform,
                VkSwapchainCreateInfo.compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                VkSwapchainCreateInfo.presentMode = Init.presentMode,
                VkSwapchainCreateInfo.clipped = True
              }
      swapchain <- managed $ Vk.withSwapchainKHR dev info Nothing bracket
      (_, images) <- Vk.getSwapchainImagesKHR dev swapchain
      return (swapchain, extent, images)

data QueueFamily = Graphics | Present

newtype QueueFamilyIndex (f :: QueueFamily) = QueueFamilyIndex {index :: Word32} deriving (Show, Eq)

withDevice :: Vk.PhysicalDevice -> QueueFamilyIndex Graphics -> QueueFamilyIndex Present -> Bool -> Managed Vk.Device
withDevice gpu (QueueFamilyIndex gfx) (QueueFamilyIndex present) portability =
  let exts =
        Vk.KHR_DYNAMIC_RENDERING_EXTENSION_NAME
          : Vk.KHR_SWAPCHAIN_EXTENSION_NAME
          : Vk.KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME
          : ([Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME | portability])
      dynamicRendering = Vk.zero {VkPhysicalDeviceDynamicRenderingFeatures.dynamicRendering = True}
      bindlessDescriptors =
        Vk.zero
          { VkPhysicalDeviceVulkan12Features.runtimeDescriptorArray = True,
            VkPhysicalDeviceVulkan12Features.descriptorBindingPartiallyBound = True,
            VkPhysicalDeviceVulkan12Features.descriptorBindingSampledImageUpdateAfterBind = True,
            VkPhysicalDeviceVulkan12Features.descriptorBindingUniformBufferUpdateAfterBind = True
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

-- TODO: break into two functions
withShaders ::
  Vk.Device ->
  Managed (Vk.SomeStruct Vk.PipelineShaderStageCreateInfo, Vk.SomeStruct Vk.PipelineShaderStageCreateInfo)
withShaders dev =
  do
    fragCode <- liftIO $ BS.readFile "out/frag.spv" -- TODO: hardcoded
    vertCode <- liftIO $ BS.readFile "out/vert.spv" -- TODO: hardcoded
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