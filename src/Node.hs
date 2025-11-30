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

data Tree = forall a. (Render a) => Leaf a | Tree Affine [Tree]

instance Render Tree where
  render :: Tree -> [Vertex PixelVec]
  render (Leaf a) = render a
  render (Tree tr as) = applyVert tr <$> (render =<< as)

tree0 :: [Tree] -> Tree
tree0 = tree mempty

node :: (Render c) => Affine -> c -> Tree
node tr c = tree tr [Leaf c]

tree :: Affine -> [Tree] -> Tree
tree = Tree