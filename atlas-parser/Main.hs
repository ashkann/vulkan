module Main (main) where

import Atlas (atlas)
import Control.Monad.Except (runExceptT)

main :: IO ()
main = do
  ae <- runExceptT $ atlas "out/atlas.atlas"
  case ae of
    Left e -> print e
    Right (_, a) -> print a
  return ()