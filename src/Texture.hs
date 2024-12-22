{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Texture
  ( Texture (..),
    BoundTexture (..),
    DescriptorIndex,
    texture,
    bindTextures,
    withHostBuffer,
    withImageView,
    withImage,
    withImageAndView
  )
where

import Codec.Picture qualified as JP
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (Managed, managed)
import Data.Bits ((.|.))
import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import Data.Word (Word32)
import Foreign (Ptr, castPtr, copyArray, withForeignPtr)
import Measure
import Utils
import Vulkan qualified as Vk
import Vulkan qualified as VkBufferCreateInfo (BufferCreateInfo (..))
import Vulkan qualified as VkDescriptorImageInfo (DescriptorImageInfo (..))
import Vulkan qualified as VkExtent3D (Extent3D (..))
import Vulkan qualified as VkImageCreateInfo (ImageCreateInfo (..))
import Vulkan qualified as VkImageSubresourceRange (ImageSubresourceRange (..))
import Vulkan qualified as VkImageViewCreateInfo (ImageViewCreateInfo (..))
import Vulkan qualified as VkWriteDescriptorSet (WriteDescriptorSet (..))
import Vulkan.CStruct.Extends qualified as Vk
import Vulkan.Zero qualified as Vk
import VulkanMemoryAllocator qualified as Vma
import VulkanMemoryAllocator qualified as VmaAllocationCreateInfo (AllocationCreateInfo (..))
import Prelude hiding (init, lookup)
import Foreign.Storable (Storable)

-- TODO remove the Texture and put everything into BoundTexture and rename it to Texture
data Texture = Texture
  { resolution :: Measure.PixelSize,
    image :: Vk.Image,
    view :: Vk.ImageView
  }
  deriving (Show)

newtype DescriptorIndex = DescriptorIndex Word32 deriving Storable

data BoundTexture = BoundTexture {index :: DescriptorIndex, texture :: Texture}

texture :: Vma.Allocator -> Vk.Device -> Vk.CommandPool -> Vk.Queue -> FilePath -> Managed Texture
texture allocator device pool queue path = do
  JP.ImageRGBA8 (JP.Image width height pixels) <- liftIO $ JP.readPng path >>= either (sayErr "Texture" . show) return
  let size = width * height * 4
      format = Vk.FORMAT_R8G8B8A8_SRGB
  (image, view) <- withImageAndView allocator device (fromIntegral width) (fromIntegral height) format
  do
    (staging, mem) <- withHostBuffer allocator (fromIntegral size)
    liftIO $ copy pixels mem size
    copyBufferToImage device pool queue staging image width height
    let res = Measure.pixelSize (fromIntegral width) (fromIntegral height)
    return Texture {resolution = res, image = image, view = view}
  where
    copy pixels mem size =
      let (src, _) = SV.unsafeToForeignPtr0 pixels
          dst = castPtr mem
       in withForeignPtr src $ \from -> copyArray dst from size

withImageAndView :: Vma.Allocator -> Vk.Device -> Word32 -> Word32 -> Vk.Format -> Managed (Vk.Image, Vk.ImageView)
withImageAndView allocator device width height format = do
  image <- withImage allocator width height format
  view <- withImageView device image format
  return (image, view)

withImage :: Vma.Allocator -> Word32 -> Word32 -> Vk.Format -> Managed Vk.Image
withImage allocator width height format = do
  (image, _, _) <-
    let dims =
          Vk.Extent3D
            { VkExtent3D.width = width,
              VkExtent3D.height = height,
              VkExtent3D.depth = 1
            }
        usage = Vk.IMAGE_USAGE_TRANSFER_SRC_BIT .|. Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT .|. Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT -- Put only what we actually need
        imgInfo =
          Vk.zero
            { VkImageCreateInfo.imageType = Vk.IMAGE_TYPE_2D,
              VkImageCreateInfo.extent = dims,
              VkImageCreateInfo.mipLevels = 1,
              VkImageCreateInfo.arrayLayers = 1,
              VkImageCreateInfo.format = format,
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

bindTextures :: Vk.Device -> Vk.DescriptorSet -> [Texture] -> Vk.Sampler -> Managed [BoundTexture]
bindTextures dev set textures sampler = do
  let info =
        Vk.SomeStruct
          Vk.zero
            { VkWriteDescriptorSet.dstSet = set,
              VkWriteDescriptorSet.dstBinding = 0, -- TODO magic number, use a configurable value
              VkWriteDescriptorSet.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
              VkWriteDescriptorSet.descriptorCount = fromIntegral $ length textures,
              VkWriteDescriptorSet.imageInfo = V.fromList $ imageInfo <$> textures,
              VkWriteDescriptorSet.dstArrayElement = 0
            }
   in Vk.updateDescriptorSets dev [info] []
  return $ zipWith (\i t -> BoundTexture {index = DescriptorIndex i, texture = t}) [0 ..] textures
  where
    imageInfo (Texture {view = v}) =
      Vk.zero
        { VkDescriptorImageInfo.imageView = v,
          VkDescriptorImageInfo.sampler = sampler,
          VkDescriptorImageInfo.imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        }