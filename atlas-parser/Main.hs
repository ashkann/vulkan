module Main (main) where

import Atlas (UAtlas, atlas, uatlas)
import Control.Monad.Except (runExceptT)

main :: IO ()
main = do
  ae <- runExceptT $ uatlas "out/atlas.atlas"
  case ae of
    Left e -> print e
    Right a -> print $ atlas a
  return ()