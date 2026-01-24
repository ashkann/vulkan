{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Node
  ( Tree,
    node,
    tree0,
    tree,
  )
where

import Affine (Affine, aff1)
import Render (Render (..))
import Vertex (Vertex, applyVert)

data Node c v = Node (Affine v v) c

data Tree v = forall c. (Render c v) => Leaf (Node c v) | Tree [Node (Tree v) v]

instance (Render c v) => Render (Node c v) v where
  render :: Node c v -> [Vertex v]
  render (Node tr c) = applyVert tr <$> render c

instance Render (Tree v) v where
  render :: Tree v -> [Vertex v]
  render (Leaf n) = render n
  render (Tree ns) = render =<< ns

tree0 :: [Tree v] -> Tree v
tree0 = tree aff1

node :: (Render c v) => Affine v v -> c -> Tree v
node tr c = Leaf (Node tr c)

tree :: Affine v v -> [Tree v] -> Tree v
tree tr children = Tree (map (Node tr) children)