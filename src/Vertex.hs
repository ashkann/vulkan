{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Vertex
  ( Vertex (..),
    vertex,
    coloredVertex,
    grpahicsPipelineVertexInputState,
    Render (..),
    opaqueColor,
    Color,
    setColor,
    noColor,
    maybeColoredVertex,
  )
where

import Data.Maybe (fromMaybe)
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

opaqueColor :: Float -> Float -> Float -> Color
opaqueColor r g b = Color $ G.vec4 r g b 1.0

data Vertex = Vertex {xy :: NDCVec, uv :: UVVec, color :: Color, texture :: Tex.DescriptorIndex}

class Render a obj | obj -> a where
  render :: a -> obj -> SV.Vector Vertex -- TODO any Traversable would do ?

setColor :: Color -> Vertex -> Vertex
setColor c v = v {color = c}

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
defaultColor = Color (G.vec4 1 1 1 1) -- Opaque white

noColor :: Color
noColor = defaultColor

vertex :: NDCVec -> UVVec -> Tex.DescriptorIndex -> Vertex
vertex xy uv tex = Vertex {xy = xy, uv = uv, texture = tex, color = defaultColor}

coloredVertex :: NDCVec -> UVVec -> Tex.DescriptorIndex -> Color -> Vertex
coloredVertex xy uv tex c = Vertex {xy = xy, uv = uv, texture = tex, color = c}

maybeColoredVertex :: NDCVec -> UVVec -> Tex.DescriptorIndex -> Maybe Color -> Vertex
maybeColoredVertex xy uv tex c = Vertex {xy = xy, uv = uv, texture = tex, color = fromMaybe noColor c}

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