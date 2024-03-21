{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}

module Ashkan2 (vulkanInit2, vulkanShutdown) where

import Ashkan (vulkanCtx)
import Foreign qualified as F
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as Cpp
import Vulkan qualified as Vk
import Prelude hiding (init)

C.context (Cpp.cppCtx <> C.funCtx <> vulkanCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_vulkan.h"
Cpp.using "namespace ImGui"

vulkanInit2 :: Vk.Instance -> Vk.PhysicalDevice -> Vk.Device -> Vk.Queue -> Vk.DescriptorPool -> IO ()
vulkanInit2 vulkan gpu device gfx p = do
  let msaaSamples = Vk.SAMPLE_COUNT_1_BIT
      descriptorPool = p
      instancePtr :: F.Ptr Vk.Instance_T
      instancePtr = Vk.instanceHandle vulkan
      physicalDevicePtr :: F.Ptr Vk.PhysicalDevice_T
      physicalDevicePtr = Vk.physicalDeviceHandle gpu
      devicePtr :: F.Ptr Vk.Device_T
      devicePtr = Vk.deviceHandle device
      queuePtr :: F.Ptr Vk.Queue_T
      queuePtr = Vk.queueHandle gfx
      minImageCount = 3
      imageCount = 3
      format = Vk.FORMAT_B8G8R8A8_SRGB
      checkResult r = putStrLn ("ImGui_ImplVulkan_Init: Vulkan Result: " <> show r)
  do
    checkResultFunPtr <- $(C.mkFunPtr [t|Vk.Result -> IO ()|]) checkResult
    putStrLn "Before calling ImGui_ImplVulkan_Init"
    res <-
      [C.block| bool {
          ImGui_ImplVulkan_InitInfo initInfo;
          VkInstance instance = { $( VkInstance_T* instancePtr ) };
          initInfo.Instance = instance;
          VkPhysicalDevice physicalDevice = { $( VkPhysicalDevice_T* physicalDevicePtr ) };
          initInfo.PhysicalDevice = physicalDevice;
          VkDevice device = { $( VkDevice_T* devicePtr ) };
          initInfo.Device = device;
          VkQueue queue = { $( VkQueue_T* queuePtr ) };
          initInfo.Queue = queue;
          initInfo.PipelineCache = VK_NULL_HANDLE;
          initInfo.DescriptorPool = $(VkDescriptorPool descriptorPool);
          initInfo.MinImageCount = $(uint32_t minImageCount);
          initInfo.ImageCount = $(uint32_t imageCount);
          initInfo.MSAASamples = $(VkSampleCountFlagBits msaaSamples);
          initInfo.CheckVkResultFn = $( void (*checkResultFunPtr)(VkResult) );
          initInfo.UseDynamicRendering = true;
          initInfo.ColorAttachmentFormat = $(VkFormat format);
          return ImGui_ImplVulkan_Init(&initInfo, VK_NULL_HANDLE );
        }|]
    putStrLn "After calling ImGui_ImplVulkan_Init"
    if res /= 0 then putStrLn "ImGui_ImplVulkan_Init successfull" else putStrLn "ImGui_ImplVulkan_Init failed"

vulkanShutdown :: IO ()
vulkanShutdown = do
  [C.exp| void { ImGui_ImplVulkan_Shutdown(); } |]
  putStrLn "ImGui_ImplVulkan_Shutdown()"