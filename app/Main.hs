{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Atlas qualified
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Managed (Managed, MonadIO (liftIO), managed, runManaged)
import Data.Bits ((.|.))
import Data.ByteString qualified as BS (readFile)
import Data.Foldable (foldlM)
import Data.Functor (($>))
import Data.Vector ((!))
import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import Foreign (Ptr, Storable, Word32, Word64)
import Foreign.Ptr (castFunPtr)
import Foreign.Storable (Storable (..), sizeOf)
import Foreign.Storable.Record qualified as Store
import Geomancy qualified as G
import Geomancy.Transform qualified as G
import Init qualified
import Measure qualified
import SDL qualified
import Texture qualified as Tex
import Utils
import Vertex qualified as Vert
import qualified Vulkan as Vk
import qualified Vulkan as VkBufferCreateInfo (BufferCreateInfo (..))
import qualified Vulkan as VkCommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan as VkDescriptorBufferInfo (DescriptorBufferInfo (..))
import qualified Vulkan as VkDescriptorPoolCreateInfo (DescriptorPoolCreateInfo (..))
import qualified Vulkan as VkDescriptorPoolSize (DescriptorPoolSize (..))
import qualified Vulkan as VkDescriptorSetAllocateInfo (DescriptorSetAllocateInfo (..))
import qualified Vulkan as VkDescriptorSetLayoutBinding (DescriptorSetLayoutBinding (..))
import qualified Vulkan as VkDescriptorSetLayoutBindingFlagsCreateInfo (DescriptorSetLayoutBindingFlagsCreateInfo (..))
import qualified Vulkan as VkDescriptorSetLayoutCreateInfo (DescriptorSetLayoutCreateInfo (..))
import qualified Vulkan as VkDevice (Device (..))
import qualified Vulkan as VkDeviceCreateInfo (DeviceCreateInfo (..))
import qualified Vulkan as VkDeviceQueueCreateInfo (DeviceQueueCreateInfo (..))
import qualified Vulkan as VkExtent2D (Extent2D (..))
import qualified Vulkan as VkFenceCreateInfo (FenceCreateInfo (..))
import qualified Vulkan as VkGraphicsPipelineCreateInfo (GraphicsPipelineCreateInfo (..))
import qualified Vulkan as VkInstance (Instance (..))
import qualified Vulkan as VkPhysicalDeviceDynamicRenderingFeatures (PhysicalDeviceDynamicRenderingFeatures (..))
import qualified Vulkan as VkPhysicalDeviceVulkan12Features (PhysicalDeviceVulkan12Features (..))
import qualified Vulkan as VkPipelineColorBlendStateCreateInfo (PipelineColorBlendStateCreateInfo (..))
import qualified Vulkan as VkPipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan as VkPipelineRenderingCreateInfo (PipelineRenderingCreateInfo (..))
import qualified Vulkan as VkPipelineShaderStageCreateInfo (PipelineShaderStageCreateInfo (..))
import qualified Vulkan as VkPipelineVertexInputStateCreateInfo (PipelineVertexInputStateCreateInfo (..))
import qualified Vulkan as VkPresentInfoKHR (PresentInfoKHR (..))
import qualified Vulkan as VkRect2D (Rect2D (..))
import qualified Vulkan as VkRenderingAttachmentInfo (RenderingAttachmentInfo (..))
import qualified Vulkan as VkRenderingInfo (RenderingInfo (..))
import qualified Vulkan as VkSamplerCreateInfo (SamplerCreateInfo (..))
import qualified Vulkan as VkShaderModuleCreateInfo (ShaderModuleCreateInfo (..))
import qualified Vulkan as VkSubmitInfo (SubmitInfo (..))
import qualified Vulkan as VkSurfaceCaps (SurfaceCapabilitiesKHR (..))
import qualified Vulkan as VkSwapchainCreateInfo (SwapchainCreateInfoKHR (..))
import qualified Vulkan as VkVPipelineMultisampleStateCreateInfo (PipelineMultisampleStateCreateInfo (..))
import qualified Vulkan as VkVertexInputAttributeDescription (VertexInputAttributeDescription (..))
import qualified Vulkan as VkVertexInputBindingDescription (VertexInputBindingDescription (..))
import qualified Vulkan as VkViewport (Viewport (..))
import qualified Vulkan as VkWriteDescriptorSet (WriteDescriptorSet (..))
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import Vulkan.CStruct.Extends qualified as Vk
import Vulkan.Dynamic qualified as Vk
import Vulkan.Zero qualified as Vk
import VulkanMemoryAllocator qualified as Vma
import VulkanMemoryAllocator qualified as VmaAllocationCreateInfo (AllocationCreateInfo (..))
import VulkanMemoryAllocator qualified as VmaAllocatorCreateInfo (AllocatorCreateInfo (..))
import Prelude hiding (init)

newtype DeltaTime = DeltaTime Float

newtype SpriteStateUpdate s = SpriteStateUpdate {update :: DeltaTime -> s -> SpriteTransformation -> (SpriteTransformation, s)}

spriteBouncingMotion :: SpriteStateUpdate G.Vec2 -- TODO: use a type for Velocity in NDC
spriteBouncingMotion = SpriteStateUpdate $ \(DeltaTime dt) v@(G.WithVec2 vx vy) s ->
  let position2@(Measure.NormalizedDeviceXY x1 y1) = Measure.ndcTranslate (v G.^* dt) s.position
      f x v = if x >= 1.0 || x <= -1.0 then -v else v
   in (s {position = position2}, G.vec2 (f x1 vx) (f y1 vy))

spriteScale :: SpriteStateUpdate G.Vec2
spriteScale = SpriteStateUpdate $ \(DeltaTime dt) v@(G.WithVec2 vx vy) s ->
  let scale2@(G.WithVec2 sx sy) = s.scale + (v G.^* dt)
      f s v = if s >= 2.0 || s <= -2.0 then -v else v
   in (s {scale = scale2}, G.vec2 (f sx vx) (f sy vy))

spriteNoMotion :: SpriteStateUpdate s
spriteNoMotion = SpriteStateUpdate $ \_ s state -> (state, s)

spriteRotation :: Float -> SpriteStateUpdate Float
spriteRotation w = SpriteStateUpdate $ \(DeltaTime dt) r state -> let r2 = r + w * dt in (state {rotation = r2}, r2)

data SpriteTransformation = SpriteTransformation
  { position :: Measure.NormalizedDevicePosition,
    rotation :: Float,
    scale :: G.Vec2
  }

-- data Sheet = Sheet {texture :: Tex.DescriptorIndex, frames :: V.Vector G.Vec4}

-- data Animation = Animation {sheet :: Sheet, speed :: Float}

-- data Animated = Animated {animation :: Animation, frameIndex :: Float}

-- $(makeLenses ''Animated)

-- loopAnimate :: Animated -> Word32 -> Animated
-- loopAnimate ani dt =
--   let new fi = fi + (ani ^. animation . speed * fromIntegral dt) / 1000
--       loop fi = if floor fi >= (ani ^. animation . sheet . frames . to V.length) then 0 else fi
--    in over frameIndex (loop . new) ani

-- TODO: reordering the fields apparently breaks the alignment. why?
data PointLight = PointLight {intensity :: Float, position :: G.Vec2, color :: G.Vec3} deriving (Show)

pointLightStore :: Store.Dictionary PointLight
pointLightStore =
  Store.run $
    PointLight
      <$> Store.element (.intensity)
      <*> Store.element (.position)
      <*> Store.element (.color)

instance Storable PointLight where
  sizeOf = Store.sizeOf pointLightStore
  alignment = Store.alignment pointLightStore
  peek = Store.peek pointLightStore
  poke = Store.poke pointLightStore

data GlobalLight = GlobalLight
  { intensity :: Float,
    _padding1 :: Word64, -- TODO: remove and make the Storable instance handle it
    _padding2 :: Word32,
    color :: G.Vec3
  }
  deriving (Show)

globalLightStore :: Store.Dictionary GlobalLight
globalLightStore =
  Store.run $
    GlobalLight
      <$> Store.element (.intensity)
      <*> Store.element (const 0)
      <*> Store.element (const 0)
      <*> Store.element (.color)

instance Storable GlobalLight where
  sizeOf = Store.sizeOf globalLightStore
  alignment = Store.alignment globalLightStore
  peek = Store.peek globalLightStore
  poke = Store.poke globalLightStore

data Viewport = Viewport
  { viewportSize :: G.UVec2,
    _padding :: Word64 -- TODO: remove and make the Storable instance handle it
  }

viewportStore :: Store.Dictionary Viewport
viewportStore =
  Store.run $
    Viewport
      <$> Store.element (.viewportSize)
      <*> Store.element (const 0)

instance Storable Viewport where
  sizeOf = Store.sizeOf viewportStore
  alignment = Store.alignment viewportStore
  peek = Store.peek viewportStore
  poke = Store.poke viewportStore

data Object s = Object
  { sprite :: Tex.Sprite,
    transformation :: SpriteTransformation,
    update :: SpriteStateUpdate s,
    s :: s
  }

data Box = forall s. Box (Object s)

data World = World
  { background :: Tex.Sprite,
    pointer :: Object (),
    objects :: [Box]
  }

frameData :: World -> FrameData
frameData world =
  FrameData
    { verts = mconcat $ uncurry vertices <$> sprites world,
      viewport = Viewport {viewportSize = G.uvec2 windowWidth windowHeight, _padding = 0}
    }

-- instance MonadError String Managed where

data Frame = Frame
  { pool :: Vk.CommandPool,
    renderCmd :: Vk.CommandBuffer,
    copyCmd :: Vk.CommandBuffer,
    fence :: Vk.Fence,
    imageAvailable :: Vk.Semaphore,
    renderFinished :: Vk.Semaphore,
    copyFinished :: Vk.Semaphore,
    staging :: (Vk.Buffer, Ptr ()),
    vertex :: Vk.Buffer,
    light :: Vk.Buffer,
    viewport :: Vk.Buffer,
    targetImage :: Vk.Image,
    targetView :: Vk.ImageView
  }

-- TODO: run inside a MonadError instance
main :: IO ()
main = runManaged $ do
  withSDL
  window <- withWindow (fromIntegral windowWidth) (fromIntegral windowHeight)
  vulkan <- withVulkan window
  _ <- withDebug vulkan
  surface <- withSurface window vulkan
  (gpu, gfx, present, portability, gpuName) <- Init.pickGPU vulkan surface
  say "Vulkan" $ "Picked GPU \"" ++ gpuName ++ "\", present queue " ++ show present ++ ", graphics queue " ++ show gfx
  device <- withDevice gpu present gfx portability <* say "Vulkan" "Created device"
  commandPool <-
    let info =
          Vk.zero
            { VkCommandPoolCreateInfo.queueFamilyIndex = gfx,
              VkCommandPoolCreateInfo.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
            }
     in managed $ Vk.withCommandPool device info Nothing bracket
  say "Vulkan" "Created swapchain"
  allocator <- withMemoryAllocator vulkan gpu device <* say "Vulkan" "Created VMA allocator"
  let descriptorCount = 16
  descSetLayout <- descriptorSetLayout device descriptorCount
  descPool <- descriptorPool device 1000
  descSet <- descriptorSet device descSetLayout descPool
  sampler <- repeatingSampler device
  gfxQueue <- Vk.getDeviceQueue device gfx 0 <* say "Vulkan" "Got graphics queue"

  atlas <- Atlas.withAtlas allocator device commandPool gfxQueue descSet sampler "out/atlas.atlas"
  say "Engine" "Atlas loaded"

  SDL.showWindow window <* say "SDL" "Show window"
  SDL.raiseWindow window <* say "SDL" "Raise window"
  (SDL.cursorVisible SDL.$= False) <* say "SDL" "Dsiable cursor"

  swapchain@(_, swapchainExtent, swapchainImages) <-
    createSwapchain
      gpu
      device
      surface
      gfx
      present
      windowWidth
      windowHeight
  let frameCount = V.length swapchainImages
  say "Engine" $ "Frame count is " ++ show frameCount

  pipelineLayout <-
    let info = Vk.zero {VkPipelineLayoutCreateInfo.setLayouts = [descSetLayout]}
     in managed $ Vk.withPipelineLayout device info Nothing bracket
  pipeline <- createPipeline device swapchainExtent pipelineLayout
  presentQueue <- Vk.getDeviceQueue device present 0 <* say "Vulkan" "Got present queue"

  let stagingBufferSize = 1048576
      maxVertCount = 1000
      vertexBufferSize = fromIntegral $ sizeOf (undefined :: Vert.Vertex) * maxVertCount
      lightBufferSize = 1024
  frames <- withFrames device gfx allocator stagingBufferSize vertexBufferSize lightBufferSize frameCount
  let shutdown = say "Engine" "Shutting down ..." *> Vk.deviceWaitIdle device
    in say "Engine" "Entering the main loop"
        *> mainLoop
          shutdown
          frameData
          ( \frameNumber frameData ->
              do
                let n = frameNumber `mod` frameCount
                    f = frames ! n
                 in frame device gfxQueue presentQueue pipeline pipelineLayout swapchain descSet f frameData
          )
          worldTime2
          worldEvent
          (world0 atlas)

world0 :: Atlas.Atlas -> World
world0 atlas =
  let one = G.vec2 1.0 1.0
      rect index piv pos =
        Object
          { sprite = Atlas.spriteIndexed atlas "rectangle" index piv windowSize,
            transformation = SpriteTransformation {position = pos, rotation = 0, scale = one},
            update = spriteNoMotion,
            s = ()
          }
      basketball =
        Object
          { sprite = Atlas.sprite atlas "basketball" Measure.texCenter windowSize,
            transformation = SpriteTransformation {position = Measure.ndcCenter, rotation = 0, scale = one},
            update = spriteBouncingMotion,
            s = G.vec2 0.1 0.2
          }

      blueBall =
        Object
          { sprite = Atlas.sprite atlas "blue_ball" Measure.texBottomLeft windowSize,
            transformation = SpriteTransformation {position = Measure.ndcCenter, rotation = 0, scale = one},
            update = spriteBouncingMotion,
            s = G.vec2 0.2 0.1
          }

      xy =
        Object
          { sprite = Atlas.sprite atlas "xy" Measure.texCenter windowSize,
            transformation = SpriteTransformation {position = Measure.ndcPos 0.5 0.5, rotation = 0, scale = one},
            update = spriteRotation $ pi / 4 * 10,
            s = 0
          }
      xy2 =
        Object
          { sprite = Atlas.sprite atlas "xy" Measure.texCenter windowSize,
            transformation = SpriteTransformation {position = Measure.ndcPos (-0.6) (-0.4), rotation = 0, scale = one},
            update = spriteScale,
            s = G.vec2 1.0 0.5
          }

      background = Atlas.sprite atlas "checkerboard" Measure.texTopLeft windowSize
      lightSource =
        Object
          { sprite = Atlas.sprite atlas "light_source" Measure.texCenter windowSize,
            transformation = SpriteTransformation {position = Measure.ndcCenter, rotation = 0, scale = one},
            update = spriteNoMotion,
            s = ()
          }
      r1 = rect 0 Measure.texTopLeft Measure.ndcTopLeft
      r2 = rect 1 Measure.texTopRight Measure.ndcTopRight
      r3 = rect 2 Measure.texBottomLeft Measure.ndcBottomLeft
      r4 = rect 3 Measure.texBottomRight Measure.ndcBottomRight
      r5 = rect 4 Measure.texCenter Measure.ndcCenter
   in World
        { background = background,
          pointer = lightSource,
          objects =
            [ Box r1,
              Box r2,
              Box r3,
              Box r4,
              Box r5,
              Box basketball,
              Box blueBall,
              Box xy,
              Box xy2
            ]
        }

data FrameData = FrameData
  { verts :: SV.Vector Vert.Vertex,
    viewport :: Viewport
  }

frame ::
  (MonadIO m) =>
  Vk.Device ->
  Vk.Queue ->
  Vk.Queue ->
  Vk.Pipeline ->
  Vk.PipelineLayout ->
  (Vk.SwapchainKHR, VkExtent2D.Extent2D, V.Vector Vk.Image) ->
  Vk.DescriptorSet ->
  Frame ->
  FrameData ->
  m ()
frame device gfxQueue presentQueue pipeline pipelineLayout swp descSet f frameData = do
  let Frame
        { pool,
          renderCmd,
          copyCmd,
          fence,
          imageAvailable,
          renderFinished,
          copyFinished,
          staging,
          vertex,
          light,
          viewport,
          targetImage,
          targetView
        } = f
      FrameData {verts, viewport = viewport'} = frameData
      (swapchain, swapchainExtent, swapchainImages) = swp
  waitForFrame device f
  liftIO $ copyToGpu2 device pool gfxQueue viewport staging viewport'
  liftIO $ copyToGpu device pool gfxQueue vertex staging verts
  -- say "Global light" $ show viewport'.globalLight
  recordRender renderCmd vertex light viewport (targetImage, targetView) swapchainExtent (fromIntegral $ SV.length verts)
  index <- Vk.acquireNextImageKHR device swapchain maxBound imageAvailable Vk.zero >>= \(r, index) -> checkSwapchainIsOld r $> index
  let swapchainImage = swapchainImages ! fromIntegral index
   in recordCopyToSwapchain copyCmd targetImage swapchainImage swapchainExtent

  let render =
        Vk.SomeStruct
          Vk.zero
            { VkSubmitInfo.commandBuffers = [Vk.commandBufferHandle renderCmd],
              VkSubmitInfo.signalSemaphores = [renderFinished]
            }
      copy =
        Vk.SomeStruct
          Vk.zero
            { VkSubmitInfo.waitSemaphores = [imageAvailable, renderFinished],
              VkSubmitInfo.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT, Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT],
              VkSubmitInfo.commandBuffers = [Vk.commandBufferHandle copyCmd],
              VkSubmitInfo.signalSemaphores = [copyFinished]
            }
   in Vk.queueSubmit gfxQueue [render, copy] fence
  let info =
        Vk.zero
          { VkPresentInfoKHR.waitSemaphores = [copyFinished],
            VkPresentInfoKHR.swapchains = [swapchain],
            VkPresentInfoKHR.imageIndices = [index]
          }
   in Vk.queuePresentKHR presentQueue info >>= checkSwapchainIsOld
  where
    checkSwapchainIsOld r = when (r == Vk.SUBOPTIMAL_KHR || r == Vk.ERROR_OUT_OF_DATE_KHR) (say "Engine" $ "presentFrame" ++ show r)
    recordCopyToSwapchain cmd offscreenImage swapchainImage extent =
      Vk.useCommandBuffer cmd Vk.zero $ do
        transitRenderTargetToCopySrc cmd offscreenImage
        transitToCopyDst cmd swapchainImage
        copyImageToImage cmd offscreenImage swapchainImage extent
        transitToPresent cmd swapchainImage
    recordRender cmd vertBuff lightBuff viewportBuff target extent vertexCount =
      Vk.useCommandBuffer cmd Vk.zero $ do
        let (image, view) = target
        Vk.cmdBindPipeline cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
        Vk.cmdBindVertexBuffers cmd 0 [vertBuff] [0]
        Vk.cmdBindDescriptorSets cmd Vk.PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 [descSet] []
        bindLights device descSet lightBuff
        bindViewport device descSet viewportBuff
        transitToRenderTarget cmd image
        let attachment =
              Vk.zero
                { VkRenderingAttachmentInfo.imageView = view,
                  VkRenderingAttachmentInfo.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                  VkRenderingAttachmentInfo.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
                  VkRenderingAttachmentInfo.storeOp = Vk.ATTACHMENT_STORE_OP_STORE,
                  VkRenderingAttachmentInfo.clearValue = clearColor
                }
            scissor = Vk.Rect2D {VkRect2D.offset = Vk.Offset2D 0 0, VkRect2D.extent = extent}
            info =
              Vk.zero
                { VkRenderingInfo.renderArea = scissor,
                  VkRenderingInfo.layerCount = 1,
                  VkRenderingInfo.colorAttachments = [attachment]
                }
            draw = Vk.cmdDraw cmd vertexCount 1 0 0
         in Vk.cmdUseRendering cmd info draw

withFrames ::
  VkDevice.Device ->
  Word32 ->
  Vma.Allocator ->
  Vk.DeviceSize ->
  Vk.DeviceSize ->
  Vk.DeviceSize ->
  Int ->
  Managed (V.Vector Frame)
withFrames device gfx allocator stagingBufferSize vertexBufferSize lightBufferSize frameCount = V.replicateM frameCount singleFrame
  where
    singleFrame =
      do
        pool <-
          let info =
                Vk.zero
                  { VkCommandPoolCreateInfo.queueFamilyIndex = gfx,
                    VkCommandPoolCreateInfo.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                  }
           in managed $ Vk.withCommandPool device info Nothing bracket
        cmds <-
          let info =
                Vk.zero
                  { Vk.commandPool = pool,
                    Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
                    Vk.commandBufferCount = 2
                  }
           in managed (Vk.withCommandBuffers device info bracket)
        vertextStagingBuffer <- Tex.withHostBuffer allocator stagingBufferSize
        imageAvailable <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
        renderFinished <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
        copyFinished <- managed $ Vk.withSemaphore device Vk.zero Nothing bracket
        fence <-
          let info = Vk.zero {VkFenceCreateInfo.flags = Vk.FENCE_CREATE_SIGNALED_BIT}
           in managed $ Vk.withFence device info Nothing bracket
        vertexBuffer <- withGPUBuffer allocator vertexBufferSize Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
        lightBuffer <- withGPUBuffer allocator lightBufferSize Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
        viewportBuffer <- withGPUBuffer allocator lightBufferSize Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
        (image, view) <- Tex.withImageAndView allocator device windowWidth windowHeight Init.imageFormat
        return
          Frame
            { pool = pool,
              renderCmd = cmds ! 0,
              copyCmd = cmds ! 1,
              fence = fence,
              imageAvailable = imageAvailable,
              renderFinished = renderFinished,
              copyFinished = copyFinished,
              staging = vertextStagingBuffer,
              vertex = vertexBuffer,
              light = lightBuffer,
              viewport = viewportBuffer,
              targetImage = image,
              targetView = view
            }

waitForFrame :: (MonadIO io) => Vk.Device -> Frame -> io ()
waitForFrame device (Frame {fence}) =
  let second = 1000000000 in Vk.waitForFences device [fence] True second *> Vk.resetFences device [fence]

mainLoop ::
  (MonadIO io) =>
  io () ->
  (w -> FrameData) ->
  (Int -> FrameData -> io ()) ->
  (DeltaTime -> w -> io w) ->
  (SDL.Event -> w -> io w) ->
  w ->
  io ()
mainLoop shutdown frameData frame worldTime worldEvent w0 =
  do
    t0 <- SDL.ticks
    go 0 t0 w0
  where
    lockFrameRate fps t1 =
      do
        t <- SDL.ticks
        let dt = t - t1
            minIdle = 1000 `div` fps
        if dt < minIdle then (liftIO . threadDelay $ 1000 * fromIntegral (minIdle - dt)) *> SDL.ticks else pure t
    go n t w = do
      -- es <- ImGui.pollEventsWithImGui
      es <- SDL.pollEvents
      if any isQuitEvent es
        then shutdown
        else do
          frame n $ frameData w
          w2 <- foldlM (flip worldEvent) w es
          t2 <- lockFrameRate 120 t
          w3 <- let dt = fromIntegral (t2 - t) in worldTime (DeltaTime (dt / 1000)) w2
          go (n + 1) t2 w3

worldEvent :: (Monad io) => SDL.Event -> World -> io World
worldEvent e w@(World {pointer = pointer@Object {transformation = state@SpriteTransformation {position = pos}}}) =
  return let p = f pos e; w1 = w {pointer = pointer {transformation = state {position = p}}} in w1
  where
    f _ (SDL.Event _ (SDL.MouseMotionEvent (SDL.MouseMotionEventData {mouseMotionEventPos = SDL.P (SDL.V2 x y)}))) =
      Measure.pixelPosToNdc (Measure.pixelPos (fromIntegral x) (fromIntegral y)) (Measure.pixelSize windowWidth windowHeight)
    f p _ = p

worldTime2 :: (Monad io) => DeltaTime -> World -> io World -- TODO: redundant?
worldTime2 dt w = do
  obj <- worldTime dt w.objects
  return w {objects = obj}

worldTime :: (Monad io) => DeltaTime -> [Box] -> io [Box]
worldTime dt = traverse (update dt)
  where
    transform dt obj = obj.update.update dt obj.s obj.transformation
    update dt (Box obj) = let (ss1, s1) = transform dt obj in return $ Box obj {transformation = ss1, s = s1}

-- update obj@Object {sprite = sprite@Sprite {pos = p0}, vel = v0, animation = _} =
--   let p1 = p0 + (v0 G.^* fromIntegral dt)
--       v1 = G.emap2 (\vi pos -> if pos >= 1.0 || pos <= -1.0 then -vi else vi) v0 p1
--    in obj {sprite = sprite {pos = p1}, vel = v1}
-- update obj@Object {sprite, animation = Just ani} =
--   let ani2 = animatedProgress ani
--    in obj {sprite = sprite {region = animatedFrame ani2}, animation = Just ani2}
-- animatedFrame :: Animated -> G.Vec4
-- animatedFrame ani = ani.animation.sheet.frames ! floor ani.frameIndex
-- animatedProgress ani =
--   let new fi = fi + ani.animation.speed * fromIntegral dt / 1000
--       loop fi = if floor fi >= V.length ani.animation.sheet.frames then 0 else fi
--    in ani {frameIndex = loop (new ani.frameIndex)}

repeatingSampler :: Vk.Device -> Managed Vk.Sampler
repeatingSampler device =
  let info =
        Vk.zero
          { VkSamplerCreateInfo.magFilter = Vk.FILTER_LINEAR,
            VkSamplerCreateInfo.minFilter = Vk.FILTER_LINEAR,
            VkSamplerCreateInfo.addressModeU = Vk.SAMPLER_ADDRESS_MODE_REPEAT,
            VkSamplerCreateInfo.addressModeV = Vk.SAMPLER_ADDRESS_MODE_REPEAT,
            VkSamplerCreateInfo.addressModeW = Vk.SAMPLER_ADDRESS_MODE_REPEAT,
            VkSamplerCreateInfo.unnormalizedCoordinates = False,
            VkSamplerCreateInfo.borderColor = Vk.BORDER_COLOR_INT_OPAQUE_WHITE
          }
   in managed $ Vk.withSampler device info Nothing bracket

windowWidth :: Word32
windowWidth = 500

windowHeight :: Word32
windowHeight = 500

windowSize :: Measure.PixelSize
windowSize = Measure.pixelSize windowWidth windowHeight

sprites :: World -> [(Tex.Sprite, SpriteTransformation)]
sprites World {background, pointer, objects} =
  (background, static) : (f <$> objects) ++ [(pointer.sprite, pointer.transformation)]
  where
    f (Box obj) = (obj.sprite, obj.transformation)
    static = SpriteTransformation {position = Measure.ndcTopLeft, rotation = 0, scale = G.vec2 1.0 1.0}

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
 
vertices :: Tex.Sprite -> SpriteTransformation -> SV.Vector Vert.Vertex
vertices
  (Tex.Sprite {texture = tex, region = Measure.UVReg u1 v1 u2 v2, size = size@(Measure.NormalizedDeviceWH w h), origin = org})
  (SpriteTransformation {position = Measure.NormalizedDeviceXY px py, rotation = rot, scale = G.WithVec2 sx sy}) =
    let a@(Measure.NormalizedDeviceXY x y) = Measure.localPosToNdc size org (Measure.ndcPos 0 0) -- TODO: improve ndc (0,0)
        b = Measure.ndcPos (x + w) y
        c = Measure.ndcPos (x + w) (y + h)
        d = Measure.ndcPos x (y + h)
        a2 = transform a
        b2 = transform b
        c2 = transform c
        d2 = transform d
        uva = Measure.uvPos u1 v1
        uvb = Measure.uvPos u2 v1
        uvc = Measure.uvPos u2 v2
        uvd = Measure.uvPos u1 v2
        topLeft = Vert.mkVertex a2 uva tex
        topRight = Vert.mkVertex b2 uvb tex
        bottomRight = Vert.mkVertex c2 uvc tex
        bottomLeft = Vert.mkVertex d2 uvd tex
     in SV.fromList
          [ topLeft,
            topRight,
            bottomRight,
            bottomRight,
            bottomLeft,
            topLeft
          ]
    where
      transform (Measure.NormalizedDeviceXY x y) =
        let translate = G.translate px py 0
            scale = G.scaleXY sx sy
            rotate = G.rotateZ rot
            G.WithVec3 x2 y2 _ = translate G.!. (scale G.!. (rotate G.!. G.vec3 x y 0)) -- TODO: order of transformations matter
         in Measure.ndcPos x2 y2

withGPUBuffer :: Vma.Allocator -> Vk.DeviceSize -> Vk.BufferUsageFlagBits -> Managed Vk.Buffer
withGPUBuffer allocator size flags = do
  (buffer, _, _) <-
    let bufferInfo =
          Vk.zero
            { VkBufferCreateInfo.size = size,
              VkBufferCreateInfo.usage = Vk.BUFFER_USAGE_TRANSFER_DST_BIT .|. flags,
              VkBufferCreateInfo.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
            }
        vmaInfo =
          Vk.zero
            { VmaAllocationCreateInfo.usage = Vma.MEMORY_USAGE_AUTO,
              VmaAllocationCreateInfo.flags = Vma.ALLOCATION_CREATE_DEDICATED_MEMORY_BIT,
              VmaAllocationCreateInfo.priority = 1
            }
     in managed $ Vma.withBuffer allocator bufferInfo vmaInfo bracket
  say "Vulkan" $ "Created GPU buffer (" ++ show size ++ " bytes)"
  return buffer

descriptorSetLayout :: Vk.Device -> Word32 -> Managed Vk.DescriptorSetLayout
descriptorSetLayout dev count = do
  let flags = Vk.DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT .|. Vk.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT
      textures =
        Vk.zero
          { VkDescriptorSetLayoutBinding.binding = 0, -- TODO: hardcoded
            VkDescriptorSetLayoutBinding.descriptorCount = count,
            VkDescriptorSetLayoutBinding.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
            VkDescriptorSetLayoutBinding.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
          }
      lights =
        Vk.zero
          { VkDescriptorSetLayoutBinding.binding = 1, -- TODO: hardcoded
            VkDescriptorSetLayoutBinding.descriptorCount = 1,
            VkDescriptorSetLayoutBinding.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            VkDescriptorSetLayoutBinding.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
          }
      viewport =
        Vk.zero
          { VkDescriptorSetLayoutBinding.binding = 2, -- TODO: hardcoded
            VkDescriptorSetLayoutBinding.descriptorCount = 1,
            VkDescriptorSetLayoutBinding.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            VkDescriptorSetLayoutBinding.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
          }
      bindings = [textures, lights, viewport]
      flagsInfo =
        Vk.zero
          { VkDescriptorSetLayoutBindingFlagsCreateInfo.bindingFlags = flags <$ bindings
          }
      layoutInfo =
        Vk.zero
          { VkDescriptorSetLayoutCreateInfo.bindings = bindings,
            VkDescriptorSetLayoutCreateInfo.flags = Vk.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
          }
          ::& flagsInfo :& ()
  managed $ Vk.withDescriptorSetLayout dev layoutInfo Nothing bracket

descriptorPool :: Vk.Device -> Word32 -> Managed Vk.DescriptorPool
descriptorPool dev textureCount =
  let poolSize typ =
        Vk.zero
          { VkDescriptorPoolSize.descriptorCount = textureCount,
            VkDescriptorPoolSize.type' = typ
          }
      types =
        [ Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
          Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
        ]
      info =
        Vk.zero
          { VkDescriptorPoolCreateInfo.poolSizes = poolSize <$> types,
            VkDescriptorPoolCreateInfo.maxSets = 1,
            VkDescriptorPoolCreateInfo.flags =
              Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT .|. Vk.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT
          }
   in managed $ Vk.withDescriptorPool dev info Nothing bracket

descriptorSet :: Vk.Device -> Vk.DescriptorSetLayout -> Vk.DescriptorPool -> Managed Vk.DescriptorSet
descriptorSet dev layout pool =
  let info =
        Vk.zero
          { VkDescriptorSetAllocateInfo.descriptorPool = pool,
            VkDescriptorSetAllocateInfo.setLayouts = [layout]
          }
   in V.head <$> managed (Vk.withDescriptorSets dev info bracket)

bindLights :: (MonadIO io) => Vk.Device -> Vk.DescriptorSet -> Vk.Buffer -> io ()
bindLights dev set lights =
  let info =
        Vk.SomeStruct
          Vk.zero
            { VkWriteDescriptorSet.dstSet = set,
              VkWriteDescriptorSet.dstBinding = 1, -- TODO: magic number, use a configurable value
              VkWriteDescriptorSet.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
              VkWriteDescriptorSet.descriptorCount = 1,
              VkWriteDescriptorSet.bufferInfo = [bufferInfo],
              VkWriteDescriptorSet.dstArrayElement = 0
            }
   in Vk.updateDescriptorSets dev [info] []
  where
    bufferInfo =
      Vk.zero
        { VkDescriptorBufferInfo.buffer = lights,
          VkDescriptorBufferInfo.offset = 0,
          VkDescriptorBufferInfo.range = Vk.WHOLE_SIZE -- TODO: better value ?
        }

bindViewport :: (MonadIO io) => Vk.Device -> Vk.DescriptorSet -> Vk.Buffer -> io ()
bindViewport dev set viewport =
  let info =
        Vk.SomeStruct
          Vk.zero
            { VkWriteDescriptorSet.dstSet = set,
              VkWriteDescriptorSet.dstBinding = 2, -- TODO: magic number, use a configurable value
              VkWriteDescriptorSet.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
              VkWriteDescriptorSet.descriptorCount = 1,
              VkWriteDescriptorSet.bufferInfo = [bufferInfo],
              VkWriteDescriptorSet.dstArrayElement = 0
            }
   in Vk.updateDescriptorSets dev [info] []
  where
    bufferInfo =
      Vk.zero
        { VkDescriptorBufferInfo.buffer = viewport,
          VkDescriptorBufferInfo.offset = 0,
          VkDescriptorBufferInfo.range = Vk.WHOLE_SIZE -- TODO: better value ?
        }

createPipeline ::
  Vk.Device ->
  Vk.Extent2D ->
  Vk.PipelineLayout ->
  Managed Vk.Pipeline
createPipeline dev extent layout = do
  (vert, frag) <- createShaders dev
  (_, res) <-
    let vertextAttribute format location offset =
          Vk.zero
            { VkVertexInputAttributeDescription.binding = 0,
              VkVertexInputAttributeDescription.location = location,
              VkVertexInputAttributeDescription.format = format,
              VkVertexInputAttributeDescription.offset = fromIntegral offset
            }
        posSize = sizeOf (undefined :: G.Vec2)
        texCordSize = sizeOf (undefined :: G.Vec2)
        attributes =
          [ vertextAttribute Vk.FORMAT_R32G32_SFLOAT 0 (0 :: Int), -- position
            vertextAttribute Vk.FORMAT_R32G32_SFLOAT 1 posSize, -- texture coordinates
            vertextAttribute Vk.FORMAT_R32_UINT 2 (posSize + texCordSize) -- texture index
          ]
        vertexInputInfo =
          Just $
            Vk.SomeStruct
              Vk.zero
                { VkPipelineVertexInputStateCreateInfo.vertexBindingDescriptions =
                    [ Vk.zero
                        { VkVertexInputBindingDescription.binding = 0,
                          VkVertexInputBindingDescription.stride = fromIntegral $ sizeOf (undefined :: Vert.Vertex),
                          VkVertexInputBindingDescription.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
                        }
                    ],
                  VkPipelineVertexInputStateCreateInfo.vertexAttributeDescriptions = attributes
                }
        dynamicRendering =
          Vk.zero
            { VkPipelineRenderingCreateInfo.colorAttachmentFormats = [Init.imageFormat]
            }
        inputAssembly =
          Just
            Vk.zero
              { Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
                Vk.primitiveRestartEnable = False
              }
        viewport =
          Just . Vk.SomeStruct $
            Vk.zero
              { Vk.viewports =
                  [ Vk.Viewport
                      { VkViewport.x = 0,
                        VkViewport.y = 0,
                        VkViewport.width = realToFrac (VkExtent2D.width extent),
                        VkViewport.height = realToFrac (VkExtent2D.height extent),
                        VkViewport.minDepth = 0,
                        VkViewport.maxDepth = 1
                      }
                  ],
                Vk.scissors =
                  [Vk.Rect2D {VkRect2D.offset = Vk.Offset2D 0 0, VkRect2D.extent = extent}]
              }
        rasterization =
          Just . Vk.SomeStruct $
            Vk.zero
              { Vk.depthClampEnable = False,
                Vk.rasterizerDiscardEnable = False,
                Vk.lineWidth = 1,
                Vk.polygonMode = Vk.POLYGON_MODE_FILL,
                Vk.cullMode = Vk.CULL_MODE_NONE,
                Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE,
                Vk.depthBiasEnable = False
              }
        multisample =
          Just . Vk.SomeStruct $
            Vk.zero
              { VkVPipelineMultisampleStateCreateInfo.sampleShadingEnable = False,
                VkVPipelineMultisampleStateCreateInfo.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT,
                VkVPipelineMultisampleStateCreateInfo.minSampleShading = 1,
                VkVPipelineMultisampleStateCreateInfo.sampleMask = [maxBound]
              }
        colorBlend =
          Just . Vk.SomeStruct $
            Vk.zero
              { VkPipelineColorBlendStateCreateInfo.logicOpEnable = False,
                VkPipelineColorBlendStateCreateInfo.attachments =
                  [ Vk.zero
                      { Vk.colorWriteMask =
                          Vk.COLOR_COMPONENT_R_BIT
                            .|. Vk.COLOR_COMPONENT_G_BIT
                            .|. Vk.COLOR_COMPONENT_B_BIT
                            .|. Vk.COLOR_COMPONENT_A_BIT,
                        Vk.srcColorBlendFactor = Vk.BLEND_FACTOR_SRC_ALPHA,
                        Vk.dstColorBlendFactor = Vk.BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                        Vk.colorBlendOp = Vk.BLEND_OP_ADD,
                        Vk.srcAlphaBlendFactor = Vk.BLEND_FACTOR_ONE,
                        Vk.dstAlphaBlendFactor = Vk.BLEND_FACTOR_ZERO,
                        Vk.alphaBlendOp = Vk.BLEND_OP_ADD,
                        Vk.blendEnable = True
                      }
                  ]
              }
        pipelineCreateInfo =
          Vk.zero
            { VkGraphicsPipelineCreateInfo.stages = [vert, frag],
              VkGraphicsPipelineCreateInfo.vertexInputState = vertexInputInfo,
              VkGraphicsPipelineCreateInfo.inputAssemblyState = inputAssembly,
              VkGraphicsPipelineCreateInfo.viewportState = viewport,
              VkGraphicsPipelineCreateInfo.rasterizationState = rasterization,
              VkGraphicsPipelineCreateInfo.multisampleState = multisample,
              VkGraphicsPipelineCreateInfo.depthStencilState = Nothing,
              VkGraphicsPipelineCreateInfo.colorBlendState = colorBlend,
              VkGraphicsPipelineCreateInfo.dynamicState = Nothing,
              VkGraphicsPipelineCreateInfo.layout = layout,
              VkGraphicsPipelineCreateInfo.subpass = 0,
              VkGraphicsPipelineCreateInfo.basePipelineHandle = Vk.zero
            }
            ::& dynamicRendering :& ()
     in managed $ Vk.withGraphicsPipelines dev Vk.zero [Vk.SomeStruct pipelineCreateInfo] Nothing bracket
  return $ V.head res

-- TODO: break into two functions
createShaders ::
  Vk.Device ->
  Managed (Vk.SomeStruct Vk.PipelineShaderStageCreateInfo, Vk.SomeStruct Vk.PipelineShaderStageCreateInfo)
createShaders dev =
  do
    fragCode <- liftIO $ BS.readFile "out/frag.spv"
    vertCode <- liftIO $ BS.readFile "out/vert.spv"
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

clearColor :: Vk.ClearValue
clearColor = Vk.Color (Vk.Float32 1.0 0.0 1.0 0)

createSwapchain ::
  Vk.PhysicalDevice ->
  Vk.Device ->
  Vk.SurfaceKHR ->
  Word32 ->
  Word32 ->
  Word32 ->
  Word32 ->
  Managed (Vk.SwapchainKHR, Vk.Extent2D, V.Vector Vk.Image)
createSwapchain
  gpu
  dev
  surface
  gfx
  present
  width
  height =
    do
      surcafeCaps <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR gpu surface
      let minImageCount = VkSurfaceCaps.minImageCount surcafeCaps
          transform = VkSurfaceCaps.currentTransform surcafeCaps
          (sharingMode, queues) =
            if gfx == present
              then (Vk.SHARING_MODE_EXCLUSIVE, [])
              else (Vk.SHARING_MODE_CONCURRENT, [gfx, present])
          extent = case VkSurfaceCaps.currentExtent surcafeCaps of
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

withDevice :: Vk.PhysicalDevice -> Word32 -> Word32 -> Bool -> Managed Vk.Device
withDevice gpu gfx present portability =
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