module Utils (say, sayErr) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

say :: (MonadIO io) => String -> String -> io ()
say prefix msg = liftIO . putStrLn $ prefix ++ ": " ++ msg

sayErr :: (MonadIO io) => String -> String -> io a
sayErr prefix msg = liftIO . throwError . userError $ prefix ++ ": " ++ msg