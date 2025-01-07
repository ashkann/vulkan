{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Vertex
  ( Vertex (..),
    mkVertex,
    vertexStore,
  )
where

import Foreign (Storable)
import Foreign.Storable (Storable (..), sizeOf)
import qualified Foreign.Storable.Record as Store
import qualified Geomancy as G
import qualified Measure
import qualified Texture as Tex
import Prelude hiding (init, lookup)

data Vertex = Vertex {xy :: G.Vec2, uv :: G.Vec2, texture :: Tex.DescriptorIndex}

mkVertex :: Measure.NormalizedDevicePosition -> Measure.TexturePosition -> Tex.DescriptorIndex -> Vertex
mkVertex pos uv index = Vertex {xy = Measure.ndcVec pos, uv = Measure.texVec uv, texture = index}

-- $(makeLenses ''Vertex)

vertexStore :: Store.Dictionary Vertex
vertexStore =
  Store.run $
    Vertex
      <$> Store.element (.xy)
      <*> Store.element (.uv)
      <*> Store.element (.texture)

-- TODO: automate the layout according to Vulkan spec
instance Storable Vertex where
  sizeOf = Store.sizeOf vertexStore
  alignment = Store.alignment vertexStore
  peek = Store.peek vertexStore
  poke = Store.poke vertexStore