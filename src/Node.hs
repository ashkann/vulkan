{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}

module Node
  ( Node,
    node1,
    node,
    node0,
  )
where

import Affine (Affine, Rotation, Scale, noRotation, noScale, origin, srt)
import Measure (PixelVec, Vec (Element, vec))
import Render (Render (..), applyObject)
import Vertex (Vertex)

data Node = forall c. (Render c) => Node {content :: c, transform :: Affine}

instance Render Node where
  render :: Node -> [Vertex PixelVec]
  render Node {content, transform} = applyObject transform content

node :: (Vec p, Render c, Element p ~ Float) => c -> Scale -> Rotation -> p -> PixelVec -> Node
node c s r p o = Node {content = c, transform = tr}
  where
    tr = Affine.srt s r p <> Affine.origin o

node1 :: (Vec p, Render c, Element p ~ Float) => c -> p -> Node
node1 c p = node c noScale noRotation p (vec 0 0)

node0 :: (Render c) => c -> Node
node0 c = Node {content = c, transform = mempty}