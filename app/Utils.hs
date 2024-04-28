{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Utils
  ( say,
    sayErr,
    withSDL,
    withVulkan,
    withSurface,
    withWindow,
    vulkanVersion,
    isQuitEvent,
    withDebug
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
import Data.Word (Word32)
import Foreign (castPtr)
import Foreign.C (peekCAString)
import SDL qualified
import SDL.Video.Vulkan qualified as SDL
import Vulkan qualified as Vk
import Vulkan qualified as VkApplicationInfo (ApplicationInfo (..))
import Vulkan qualified as VkDebugUtilsMessengerCreateInfoEXT (DebugUtilsMessengerCreateInfoEXT (..))
import Vulkan qualified as VkInstanceCreateInfo (InstanceCreateInfo (..))
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import Vulkan.Utils.Debug qualified as Vk
import Vulkan.Zero qualified as Vk

say :: (MonadIO io) => String -> String -> io ()
say prefix msg = liftIO . putStrLn $ prefix ++ ": " ++ msg

sayErr :: (MonadIO io) => String -> String -> io a
sayErr prefix msg = liftIO . throwError . userError $ prefix ++ ": " ++ msg

applicationName :: (IsString a) => a
applicationName = "Vulkan 2D Engine"

vulkanVersion :: Word32
vulkanVersion = Vk.API_VERSION_1_2

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
  say "Vulkan" $ "Instance extenions: " ++ unwords (BS.unpack <$> exts)
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
          ::& debugUtilsMessengerCreateInfo
            :& Vk.ValidationFeaturesEXT
              [ -- Vk.VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT,
                Vk.VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT -- ,
                -- Vk.VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT
              ]
              []
            :& ()
   in managed $ Vk.withInstance info Nothing bracket

debugUtilsMessengerCreateInfo :: Vk.DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo =
  Vk.zero
    { Vk.messageSeverity =
        Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
      -- .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT,
      Vk.messageType =
        Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
      VkDebugUtilsMessengerCreateInfoEXT.pfnUserCallback = Vk.debugCallbackPtr
    }

withDebug :: Vk.Instance -> Managed ()
withDebug vulkan = do
  _ <- managed $ Vk.withDebugUtilsMessengerEXT vulkan debugUtilsMessengerCreateInfo Nothing bracket
  Vk.submitDebugUtilsMessageEXT
    vulkan
    Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
    Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
    Vk.zero {Vk.message = "Debug Message Test"}    

withSurface :: SDL.Window -> Vk.Instance -> Managed Vk.SurfaceKHR
withSurface w v@(Vk.Instance v' _) = managed $ bracket create destroy
  where
    destroy s = Vk.destroySurfaceKHR v s Nothing <* say "Vulkan" "Destroyed surface"
    create = Vk.SurfaceKHR <$> SDL.vkCreateSurface w (castPtr v') <* say "SDL" "Vulkan surface created"

withWindow :: Int -> Int -> Managed SDL.Window
withWindow width height =
  managed $
    bracket
      (SDL.createWindow applicationName win <* say "SDL" "Window created")
      (\w -> SDL.destroyWindow w <* say "SDL" "Window destroyed")
  where
    size = SDL.V2 (fromIntegral width) (fromIntegral height)
    win =
      SDL.defaultWindow
        { SDL.windowInitialSize = size,
          SDL.windowGraphicsContext = SDL.VulkanContext,
          SDL.windowPosition = SDL.Centered
        }

withSDL :: Managed ()
withSDL = do
  liftIO printVersion
  with_ (SDL.initialize flags <* say "SDL" "Initialized") (SDL.quit <* say "SDL" "Quit")
  with_ (SDL.vkLoadLibrary Nothing <* say "SDL" "Loaded Vulkan lib") (SDL.vkUnloadLibrary <* say "SDL" "Unloaded Vulkan lib")
  where
    with_ acq rel = managed_ $ bracket_ acq rel
    printVersion = SDL.version >>= (\(v0 :: Int, v1, v2) -> putStrLn $ "SDL: Version " ++ show v0 ++ "." ++ show v1 ++ "." ++ show v2)
    flags = [SDL.InitEvents :: SDL.InitFlag, SDL.InitVideo] :: [SDL.InitFlag]

isQuitEvent :: SDL.Event -> Bool
isQuitEvent = \case
  (SDL.Event _ SDL.QuitEvent) -> True
  SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released False (SDL.Keysym _ code _)))
    | code == SDL.KeycodeQ || code == SDL.KeycodeEscape ->
        True
  _ -> False