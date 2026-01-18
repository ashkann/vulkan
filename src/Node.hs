{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}

module Node
  ( Tree,
    node,
    tree0,
    tree,
  )
where

import Affine (Affine)
import Measure (PixelVec)
import Render (Render (..))
import Vertex (Vertex, applyVert)

data Node c = Node Affine c

data Tree = forall c. (Render c) => Leaf (Node c) | Tree [Node Tree]

instance (Render c) => Render (Node c) where
  render :: Node c -> [Vertex PixelVec]
  render (Node tr c) = applyVert tr <$> render c

instance Render Tree where
  render :: Tree -> [Vertex PixelVec]
  render (Leaf n) = render n
  render (Tree ns) = render =<< ns

tree0 :: [Tree] -> Tree
tree0 = tree mempty

node :: (Render c) => Affine -> c -> Tree
node tr c = Leaf (Node tr c)

tree :: Affine -> [Tree] -> Tree
tree tr children = Tree (map (Node tr) children)