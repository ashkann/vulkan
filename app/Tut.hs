{-# LANGUAGE TemplateHaskell #-}

module Tut where

import Control.Lens

data Atom = Atom {_element :: String, _point :: Point}

data Point = Point {_x :: Double, _y :: Double}

data Molecule = Molecule {_atoms :: [Atom]}

makeLenses ''Atom
makeLenses ''Point
makeLenses ''Molecule

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

shiftMolculeX :: Molecule -> Molecule
shiftMolculeX = over (atoms . traverse . point . x) (+ 1)
