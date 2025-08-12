{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Vertex
  ( Vertex (..),
    vertex,
    vertexColored,
    grpahicsPipelineVertexInputState,
    Quad,
    quad,
    vertices,
    Render(..),
  )
where

import qualified Data.Vector.Storable as SV
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
import qualified Vulkan as VkVertexInputBindingDescription (VertexInputBindingDescription (..))
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk

newtype Color = Color G.Vec4 deriving (Show, Storable)

data Vertex = Vertex {xy :: NDCVec, uv :: UVVec, color :: Color, texture :: Tex.DescriptorIndex}

data Quad = Quad {a, b, c, d :: Vertex}

quad ::
  -- | Top left cornder, a
  Vertex ->
  -- | Top right cornder, b
  Vertex ->
  -- | Bottom right cornder, c
  Vertex ->
  -- | Bottom left cornder, d
  Vertex ->
  Quad
quad a b c d = Quad {a, b, c, d}

vertices :: Quad -> SV.Vector Vertex
vertices (Quad a b c d) = [a, b, c, c, d, a]

class Render m s where
  render :: s -> m Quad

vertexStore :: Store.Dictionary Vertex
vertexStore =
  Store.run $
    Vertex
      <$> Store.element (.xy)
      <*> Store.element (.uv)
      <*> Store.element (.color)
      <*> Store.element (.texture)

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
        Vk.vertexAttributeDescriptions =
          [ attr Vk.FORMAT_R32G32_SFLOAT 0 positionOffset,
            attr Vk.FORMAT_R32G32_SFLOAT 1 uvOffset,
            attr Vk.FORMAT_R32G32B32A32_SFLOAT 2 colorOffset,
            attr Vk.FORMAT_R32_UINT 3 textureIndexOffset
          ]
      }
  where
    attr format location offset =
      Vk.zero
        { Vk.binding = 0,
          Vk.location = location,
          Vk.format = format,
          Vk.offset = fromIntegral offset
        }
    positionOffset = 0
    uvOffset = positionOffset + sizeOf (undefined :: NDCVec)
    colorOffset = uvOffset + sizeOf (undefined :: UVVec)
    textureIndexOffset = colorOffset + sizeOf (undefined :: Color)