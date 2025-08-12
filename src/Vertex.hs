{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vertex
  ( Vertex (..),
  vertex,
  vertexColored)
where

import Foreign (Storable)
import Foreign.Storable (Storable (..), sizeOf)
import qualified Foreign.Storable.Record as Store
import Measure
import qualified Texture as Tex
-- import Prelude hiding (init, lookup)
import qualified Geomancy as G

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
vertex xy uv tex = Vertex { xy = xy, uv = uv, texture = tex, color = defaultColor}

vertexColored :: NDCVec -> UVVec -> Tex.DescriptorIndex -> Color -> Vertex
vertexColored xy uv tex c = Vertex { xy = xy, uv = uv, texture = tex, color = c}