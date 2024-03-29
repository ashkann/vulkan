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
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}

module Ashkan (vulkanCtx) where

import Data.Map.Strict qualified as Map (fromList)
import Language.C.Inline qualified as C
import Language.C.Inline.Context qualified as C
import Language.C.Types qualified as C
import Vulkan qualified as Vk
import Prelude hiding (init)

vulkanTypesTable :: C.TypesTable
vulkanTypesTable =
  Map.fromList
    [ (C.TypeName "VkAllocationCallbacks", [t|Vk.AllocationCallbacks|]),
      (C.TypeName "VkCommandBuffer_T", [t|Vk.CommandBuffer_T|]),
      (C.TypeName "VkDescriptorPool", [t|Vk.DescriptorPool|]),
      (C.TypeName "VkDevice_T", [t|Vk.Device_T|]),
      (C.TypeName "VkInstance_T", [t|Vk.Instance_T|]),
      (C.TypeName "VkPhysicalDevice_T", [t|Vk.PhysicalDevice_T|]),
      (C.TypeName "VkPipeline", [t|Vk.Pipeline|]),
      (C.TypeName "VkPipelineCache", [t|Vk.PipelineCache|]),
      (C.TypeName "VkQueue_T", [t|Vk.Queue_T|]),
      (C.TypeName "VkRenderPass", [t|Vk.RenderPass|]),
      (C.TypeName "VkResult", [t|Vk.Result|]),
      (C.TypeName "VkSampleCountFlagBits", [t|Vk.SampleCountFlagBits|]),
      (C.TypeName "VkSampler", [t|Vk.Sampler|]),
      (C.TypeName "VkImageView", [t|Vk.ImageView|]),
      (C.TypeName "VkImageLayout", [t|Vk.ImageLayout|]),
      (C.TypeName "VkDescriptorSet", [t|Vk.DescriptorSet|]),
      (C.TypeName "VkFormat", [t|Vk.Format|])
    ]

vulkanCtx :: C.Context
vulkanCtx = mempty {C.ctxTypesTable = vulkanTypesTable}