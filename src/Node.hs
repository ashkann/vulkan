{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Node
  ( Node,
    node0,
    node,
  )
where

import Affine (Affine)
import Measure (PixelVec)
import Render (Render (..), applyObject)
import Vertex (Vertex)

data Node = forall c. (Render c) => Node {content :: c, transform :: Affine}

instance Render Node where
  render :: Node -> [Vertex PixelVec]
  render Node {content, transform} = applyObject transform content

node0 :: (Render c) => c -> Node
node0 c = Node {content = c, transform = mempty}

node :: (Render c) => c -> Affine -> Node
node c tr = Node {content = c, transform = tr}