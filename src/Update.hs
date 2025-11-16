module Update (Update (..), TimeSeconds (..), timeSecondsFromMillis) where

import qualified SDL.Event as SDL

newtype TimeSeconds = TimeSeconds Float

timeSecondsFromMillis :: (Integral t) => t -> t -> TimeSeconds
timeSecondsFromMillis t2 t = let dt = fromIntegral (t2 - t) in TimeSeconds (dt / 1000)

class Update obj where
  event :: obj -> [SDL.Event] -> obj
  time :: obj -> TimeSeconds -> obj
  {-# MINIMAL event, time #-}