{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Init (suitable, pickGPU, presentMode, imageFormat, colorSpace) where

import Control.Monad.Managed
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import Data.Bits (Bits (zeroBits), (.&.))
import Data.Foldable (foldlM)
import Data.Functor (($>), (<&>))
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

pickGPU ::
  Vk.Instance ->
  Vk.SurfaceKHR ->
  Managed (Maybe (Vk.PhysicalDevice, Word32, Word32, Bool))
pickGPU vulkan surface = do
  (_, gpus) <- Vk.enumeratePhysicalDevices vulkan
  let isGood d = ExceptT $ find d <&> (\case Nothing -> Right (); Just ((g, p), pss) -> Left (d, g, p, pss))
   in findM (\_ gpu -> isGood gpu) () gpus
  where
    findM f s0 ta = runExceptT (foldlM f s0 ta) >>= (\r -> return $ case r of Left b -> Just b; Right _ -> Nothing)
    -- found = ExceptT . return . Left
    -- continue = ExceptT . return . Right

    support yes feature = liftIO (say "Enging" msg $> out)
      where
        msg = "GPU supports " ++ feature ++ ": " ++ show yes
        out = if yes then Just () else Nothing

    find :: Vk.PhysicalDevice -> Managed (Maybe ((Word32, Word32), Bool))
    find gpu = do
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
          queueFamilyIndices <- Init.suitable gpu surface
          return $ (,portabilitySubSetPresent exts) <$> queueFamilyIndices
        else return Nothing
      where
        dynamicRenderingSupported = V.any ((== Vk.KHR_DYNAMIC_RENDERING_EXTENSION_NAME) . Vk.extensionName)

        swapchainSupported exts = do
          (_, formats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR gpu surface
          (_, modes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR gpu surface
          let isGood f = VkSurfaceFormat.format f == imageFormat && VkSurfaceFormat.colorSpace f == colorSpace
              swapChain =
                any ((== Vk.KHR_SWAPCHAIN_EXTENSION_NAME) . Vk.extensionName) exts
                  && V.any isGood formats
                  && V.any (== presentMode) modes
          return swapChain

        portabilitySubSetPresent = any ((== Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME) . Vk.extensionName)

data QueueLookup = NoneFound | GfxFound Word32 | PresentFound Word32

suitable :: forall io. (MonadIO io) => Vk.PhysicalDevice -> Vk.SurfaceKHR -> io (Maybe (Word32, Word32))
suitable gpu surface = do
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