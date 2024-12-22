#!/usr/bin/env sh
# export MVK_CONFIG_USE_METAL_ARGUMENT_BUFFERS=1
# export VK_KHRONOS_VALIDATION_ENABLES=VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT
# export VK_LAYER_SETTINGS_PATH=$PWD
# Uncomment if you need to repack the textures or running for the first time
# ./pack-textures.sh
./compile-shaders.sh
stack run vulkan2dengine