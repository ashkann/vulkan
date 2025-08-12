{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Vertex
  ( Vertex (..),
    vertex,
    vertexColored,
    grpahicsPipelineVertexInputState,
  )
where

import Foreign (Storable)
import Foreign.Storable (Storable (..), sizeOf)
import qualified Foreign.Storable.Record as Store
import qualified Geomancy as G
import Measure
import qualified Texture as Tex
import qualified Vulkan as Vk
  ( Format (..),
    PipelineVertexInputStateCreateInfo (..),
    VertexInputAttributeDescription (..),
    VertexInputRate (VERTEX_INPUT_RATE_VERTEX),
  )
-- import qualified Vulkan as VkVertexInputAttributeDescription (VertexInputAttributeDescription (..))
import qualified Vulkan as VkVertexInputBindingDescription (VertexInputBindingDescription (..))
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk

newtype Color = Color G.Vec4 deriving (Show, Storable)

data Vertex = Vertex {xy :: NDCVec, uv :: UVVec, texture :: Tex.DescriptorIndex, color :: Color}

vertexStore :: Store.Dictionary Vertex
vertexStore =
  Store.run $
    Vertex
      <$> Store.element (.xy)
      <*> Store.element (.uv)
      <*> Store.element (.texture)
      <*> Store.element (.color)

-- TODO: automate the layout according to Vulkan spec
instance Storable Vertex where
  sizeOf = Store.sizeOf vertexStore
  alignment = Store.alignment vertexStore
  peek = Store.peek vertexStore
  poke = Store.poke vertexStore

defaultColor :: Color
defaultColor = Color (G.vec4 1 1 1 1) -- Opaue white

vertex :: NDCVec -> UVVec -> Tex.DescriptorIndex -> Vertex
vertex xy uv tex = Vertex {xy = xy, uv = uv, texture = tex, color = defaultColor}

vertexColored :: NDCVec -> UVVec -> Tex.DescriptorIndex -> Color -> Vertex
vertexColored xy uv tex c = Vertex {xy = xy, uv = uv, texture = tex, color = c}

grpahicsPipelineVertexInputState :: Vk.SomeStruct Vk.PipelineVertexInputStateCreateInfo
grpahicsPipelineVertexInputState =
  Vk.SomeStruct
    Vk.zero
      { Vk.vertexBindingDescriptions =
          [ Vk.zero
              { VkVertexInputBindingDescription.binding = 0,
                VkVertexInputBindingDescription.stride = fromIntegral $ sizeOf (undefined :: Vertex),
                VkVertexInputBindingDescription.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
              }
          ],
        Vk.vertexAttributeDescriptions = attributes
      }
  where
    vertextAttribute format location offset =
      Vk.zero
        { Vk.binding = 0,
          Vk.location = location,
          Vk.format = format,
          Vk.offset = fromIntegral offset
        }
    posSize = sizeOf (undefined :: NDCVec)
    texCordSize = sizeOf (undefined :: UVVec)
    texIndexSize = sizeOf (undefined :: Tex.DescriptorIndex)
    attributes =
      [ vertextAttribute Vk.FORMAT_R32G32_SFLOAT 0 (0 :: Int), -- position
        vertextAttribute Vk.FORMAT_R32G32_SFLOAT 1 posSize, -- texture coordinates
        vertextAttribute Vk.FORMAT_R32_UINT 2 (posSize + texCordSize), -- texture index
        vertextAttribute Vk.FORMAT_R32G32B32A32_SFLOAT 3 (posSize + texCordSize + texIndexSize) -- color
      ]