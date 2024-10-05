{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Init (suitable) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Bits (Bits (zeroBits), (.&.))
import Data.Functor ((<&>))
import qualified Data.Vector as V
import Data.Word (Word32)
import qualified Vulkan as Vk

data QueueLookup = NoneFound | GfxFound Word32 | PresentFound Word32

suitable :: forall io. (MonadIO io) => Vk.PhysicalDevice -> Vk.SurfaceKHR -> MaybeT io (Word32, Word32)
suitable gpu surface = do
  qs <- Vk.getPhysicalDeviceQueueFamilyProperties gpu
  let res = runExceptT $ V.ifoldM lookup NoneFound qs
      out = either Just (const Nothing) <$> res
   in MaybeT out
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
