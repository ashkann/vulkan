{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Init (pickGPU, presentMode, imageFormat, colorSpace) where

import Control.Monad.Managed
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Bits (Bits (zeroBits), (.&.))
import Data.Functor (($>), (<&>))
import Data.Monoid (Alt (Alt))
import qualified Data.Vector as V
import Data.Word (Word32)
import Utils (say)
import qualified Vulkan as Vk
import qualified Vulkan as VkPhysicalDeviceFeatures2 (PhysicalDeviceFeatures2 (..))
import qualified Vulkan as VkPhysicalDeviceVulkan12Features (PhysicalDeviceVulkan12Features (..))
import qualified Vulkan as VkSurfaceFormat (SurfaceFormatKHR (..))

-- newtype QueueFamilyIndex = QueueFamilyIndex Word32

presentMode :: Vk.PresentModeKHR
presentMode = Vk.PRESENT_MODE_FIFO_KHR

imageFormat :: Vk.Format
imageFormat = Vk.FORMAT_B8G8R8A8_SRGB

colorSpace :: Vk.ColorSpaceKHR
colorSpace = Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR

data QueueLookup = NoneFound | GfxFound Word32 | PresentFound Word32

pickGPU ::
  forall io.
  (MonadIO io) =>
  Vk.Instance ->
  Vk.SurfaceKHR ->
  io (Maybe (Vk.PhysicalDevice, Word32, Word32, Bool))
pickGPU vulkan surface = do
  (_, gpus) <- Vk.enumeratePhysicalDevices vulkan
  runMaybeT $ do
    let Alt maybeGpu = V.foldMap (\gpu -> Alt $ (gpu,) <$> suitable gpu) gpus
    (gpu, ((gfx, present), portability)) <- maybeGpu
    return (gpu, gfx, present, portability)
  where
    -- TODO get rid of the `lift` s?
    suitable :: Vk.PhysicalDevice -> MaybeT io ((Word32, Word32), Bool)
    suitable gpu = do
      features <- lift (Vk.getPhysicalDeviceFeatures2 gpu :: io (Vk.PhysicalDeviceFeatures2 '[Vk.PhysicalDeviceVulkan12Features]))
      _ <-
        let f = fst $ VkPhysicalDeviceFeatures2.next features
         in do
              _ <- supported (VkPhysicalDeviceVulkan12Features.runtimeDescriptorArray f) "runtimeDescriptorArray"
              _ <- supported (VkPhysicalDeviceVulkan12Features.descriptorBindingPartiallyBound f) "descriptorBindingPartiallyBound"
              _ <- supported (VkPhysicalDeviceVulkan12Features.descriptorBindingSampledImageUpdateAfterBind f) "descriptorBindingSampledImageUpdateAfterBind"
              supported True "Bindless Descriptors"
      (_, exts) <- Vk.enumerateDeviceExtensionProperties gpu Nothing
      swpChain <- swapchainSupported exts
      if swpChain && dynamicRenderingSupported exts
        then do
          maybeIndecies <- lift findQueueFamilyIndecies
          MaybeT . return $ ((,portabilitySubSetPresent exts) <$> maybeIndecies)
        else MaybeT $ return Nothing
      where
        -- TODO encode theerror in a monad
        supported isSupported feature = liftIO (say "Enging" msg $> out)
          where
            msg = "GPU supports " ++ feature ++ ": " ++ show isSupported
            out = if isSupported then Just () else Nothing

        dynamicRenderingSupported = V.any ((== Vk.KHR_DYNAMIC_RENDERING_EXTENSION_NAME) . Vk.extensionName)

        portabilitySubSetPresent = any ((== Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME) . Vk.extensionName)

        swapchainSupported exts = do
          (_, formats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR gpu surface
          (_, modes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR gpu surface
          let isGood f = VkSurfaceFormat.format f == imageFormat && VkSurfaceFormat.colorSpace f == colorSpace
              swapChain =
                any ((== Vk.KHR_SWAPCHAIN_EXTENSION_NAME) . Vk.extensionName) exts
                  && V.any isGood formats
                  && V.any (== presentMode) modes
          return swapChain

        findQueueFamilyIndecies :: io (Maybe (Word32, Word32))
        findQueueFamilyIndecies = do
          qs <- Vk.getPhysicalDeviceQueueFamilyProperties gpu
          either Just (const Nothing) <$> runExceptT (V.ifoldM lookup NoneFound qs)
          where
            lookup :: QueueLookup -> Int -> Vk.QueueFamilyProperties -> ExceptT (Word32, Word32) io QueueLookup
            lookup acc _ q | Vk.queueCount q <= 0 = pure acc
            lookup acc index q =
              let i = fromIntegral index
                  maybeGfx = pick $ (Vk.QUEUE_GRAPHICS_BIT .&. Vk.queueFlags q) /= zeroBits
                  maybePresent = Vk.getPhysicalDeviceSurfaceSupportKHR gpu i surface <&> pick
                  pick b = if b then Just i else Nothing
                  (gfx, ioPresent) =
                    case acc of
                      GfxFound gfx -> (Just gfx, maybePresent)
                      PresentFound present -> (maybeGfx, pure (Just present))
                      NoneFound -> (maybeGfx, maybePresent)
               in do
                    present <- ioPresent
                    case (gfx, present) of
                      (Just gfx, Just present) -> throwE (gfx, present)
                      (Just gfx, Nothing) -> pure $ GfxFound gfx
                      (Nothing, Just present) -> pure $ PresentFound present
                      (Nothing, Nothing) -> pure NoneFound