{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Node
  ( Tree,
    Node,
    node0,
    node,
    tree0,
    tree,
  )
where

import Affine (Affine)
import Measure (PixelVec)
import Render (Render (..))
import Vertex (Vertex, applyVert)

data Node c = Node {content :: c, transform :: Affine}

data Tree = forall c. (Render c) => Leaf (Node c) | Parent (Node [Tree])

instance Render Tree where
  render :: Tree -> [Vertex PixelVec]
  render (Leaf (Node obj tr)) = applyVert tr <$> render obj
  render (Parent (Node children tr)) = applyVert tr <$> (render =<< children)

node0 :: (Render c) => c -> Tree
node0 = node mempty

tree0 :: [Tree] -> Tree
tree0 = tree mempty

node :: (Render c) => Affine -> c -> Tree
node tr c = Leaf (Node {content = c, transform = tr})

tree :: Affine -> [Tree] -> Tree
tree tr children = Parent (Node {content = children, transform = tr})