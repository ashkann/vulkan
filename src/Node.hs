{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Node
  ( Tree,
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

data Tree = forall a. (Render a) => Leaf a | Parent Affine [Tree]

instance Render Tree where
  render :: Tree -> [Vertex PixelVec]
  render (Leaf a) = render a
  render (Parent tr a) = applyVert tr <$> (render =<< a)

node0 :: (Render a) => a -> Tree
node0 = Leaf

tree0 :: [Tree] -> Tree
tree0 = tree mempty

node :: (Render c) => Affine -> c -> Tree
node tr c = tree tr [node0 c]

tree :: Affine -> [Tree] -> Tree
tree = Parent