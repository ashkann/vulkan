{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Vertex
  ( Vertex,
    vertex,
    colorVertex,
    grpahicsPipelineVertexInputState,
    opaqueColor,
    Color,
    setColor,
    noColor,
    defaultColor,
    setXY,
    applyVert,
  )
where

import Affine (Affine, applyVec)
import Data.Maybe (fromMaybe)
import Foreign (Storable)
import Foreign.Storable (Storable (..), sizeOf)
import qualified Foreign.Storable.Record as Store
import qualified Geomancy as G
import Measure
import Texture (DescriptorIndex)
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

data Vertex v = Vertex {xy :: v, uv :: UVVec, color :: Color, texture :: DescriptorIndex}

setColor :: Color -> Vertex v -> Vertex v
setColor c v = v {color = c}

setXY :: v -> Vertex u -> Vertex v
setXY xy vert = vert {xy = xy}

applyVert :: (Vec u, Vec v, Element u ~ Float, Element v ~ Float) => Affine -> Vertex u -> Vertex v
applyVert tr vert = setXY xy' vert
  where
    xy' = applyVec tr vert.xy

vertexStore :: Store.Dictionary (Vertex NDCVec)
vertexStore =
  Store.run $
    Vertex
      <$> Store.element (.xy)
      <*> Store.element (.uv)
      <*> Store.element (.color)
      <*> Store.element (.texture)

-- TODO: automate the layout according to Vulkan spec
instance Storable (Vertex NDCVec) where
  sizeOf = Store.sizeOf vertexStore
  alignment = Store.alignment vertexStore
  peek = Store.peek vertexStore
  poke = Store.poke vertexStore

defaultColor :: Color
defaultColor = Color (G.vec4 1 1 1 1) -- Opaque white

noColor :: Color
noColor = defaultColor

vertex :: v -> UVVec -> DescriptorIndex -> Vertex v
vertex xy uv tex = Vertex {xy = xy, uv = uv, texture = tex, color = defaultColor}

colorVertex :: v -> UVVec -> DescriptorIndex -> Maybe Color -> Vertex v
colorVertex xy uv tex c = Vertex {xy = xy, uv = uv, texture = tex, color = fromMaybe noColor c}

grpahicsPipelineVertexInputState :: Vk.SomeStruct Vk.PipelineVertexInputStateCreateInfo
grpahicsPipelineVertexInputState =
  Vk.SomeStruct
    Vk.zero
      { Vk.vertexBindingDescriptions =
          [ Vk.zero
              { VkVertexInputBindingDescription.binding = 0,
                VkVertexInputBindingDescription.stride = fromIntegral $ sizeOf (undefined :: Vertex NDCVec),
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