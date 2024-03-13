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

module Ashkan2 (vulkanInit, vulkanShutdown) where

import Ashkan (vulkanCtx)
import Foreign (Ptr)
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as Cpp
import Vulkan qualified as Vk
import Prelude hiding (init)

C.context (Cpp.cppCtx <> C.funCtx <> vulkanCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_vulkan.h"
Cpp.using "namespace ImGui"

vulkanInit :: Vk.Instance -> Vk.PhysicalDevice -> Vk.Device -> Vk.Queue -> Vk.DescriptorPool -> IO ()
vulkanInit vulkan gpu device gfx p = do
  let instancePtr :: Ptr Vk.Instance_T
      instancePtr = Vk.instanceHandle vulkan
      physicalDevicePtr :: Ptr Vk.PhysicalDevice_T
      physicalDevicePtr = Vk.physicalDeviceHandle gpu
      devicePtr :: Ptr Vk.Device_T
      devicePtr = Vk.deviceHandle device
      queuePtr :: Ptr Vk.Queue_T
      queuePtr = Vk.queueHandle gfx
      msaaSamples = Vk.SAMPLE_COUNT_1_BIT
      renderPass = Vk.NULL_HANDLE
      pool = p
      format = Vk.FORMAT_B8G8R8A8_SRGB
  do
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
              initInfo.DescriptorPool = $(VkDescriptorPool pool);
              initInfo.MinImageCount = 3;
              initInfo.ImageCount = 3;
              initInfo.MSAASamples = $(VkSampleCountFlagBits msaaSamples);
              initInfo.UseDynamicRendering = true;
              initInfo.ColorAttachmentFormat = $(VkFormat format);
              return ImGui_ImplVulkan_Init(&initInfo, $(VkRenderPass renderPass) );
            }|]
    putStrLn "After calling ImGui_ImplVulkan_Init"
    if res /= 0 then putStrLn "ImGui_ImplVulkan_Init successfull" else putStrLn "ImGui_ImplVulkan_Init failed"

vulkanShutdown :: IO ()
vulkanShutdown = [C.exp| void { ImGui_ImplVulkan_Shutdown(); } |]