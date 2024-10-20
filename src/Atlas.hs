{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Atlas
  ( atlas,
    lookup,
    lookupIndexed,
    Atlas (..),
    Region (..),
    Key (..),
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Word (Word32)
import qualified Geomancy as G
import Text.Parsec (anyChar, char, digit, endOfLine, eof, many1, manyTill, optionMaybe, parse, string, (<?>))
import Text.Parsec.Char (spaces)
import Text.Parsec.String (Parser, parseFromFile)
import Prelude hiding (lookup)

newtype Region = Region G.Vec4 deriving (Show)

newtype Key = Key (String, Maybe Word32)
  deriving (Show)
  deriving (Ord, Eq)

newtype Atlas = Atlas (M.Map Key Region) deriving (Show)

lookup :: Atlas -> String -> Region
lookup (Atlas rs) name = rs M.! Key (name, Nothing)

lookupIndexed :: Atlas -> String -> Word32 -> Region
lookupIndexed (Atlas rs) name index = rs M.! Key (name, Just index)

atlas :: (MonadError String m, MonadIO m) => FilePath -> m (FilePath, Atlas)
atlas file = liftIO (parseFromFile parser file) >>= either (throwError . show) return

-- atlas :: Atlas -> Atlas
-- atlas (Atlas _ regions (G.WithUVec2 aw ah)) =
--   Atlas $ (\(URegion (G.WithUVec4 x1 y1 x2 y2)) -> Region $ G.vec4 (u x1) (v y1) (u x2) (v y2)) <$> regions
--   where
--     u x = fromIntegral x / fromIntegral aw
--     v y = fromIntegral y / fromIntegral ah

parser :: Parser (FilePath, Atlas)
parser = do
  (fileName, s) <- headerP <?> "header"
  regions <- manyTill (regionP <&> \(k, xy, size) -> (k, toRegion s xy size)) eof
  return (fileName, Atlas $ M.fromList regions)

toRegion :: G.UVec2 -> G.UVec2 -> G.UVec2 -> Region
toRegion (G.WithUVec2 sx sy) (G.WithUVec2 x y) (G.WithUVec2 w h) = Region $ G.vec4 (u x) (v y) (u $ x + w) (v $ y + h)
  where
    u x = fromIntegral x / fromIntegral sx
    v y = fromIntegral y / fromIntegral sy

-- | Parse the header

{-
atlas.png
size: 3165, 2052
format: RGBA8888
filter: Nearest, Nearest
repeat: none
-}
headerP :: Parser (FilePath, G.UVec2)
headerP = do
  _ <- endOfLine <?> "empty line"
  fileName <- manyTill anyChar endOfLine <?> "fileName"
  size <- vartP "size" (const uvec2P)
  _ <- vartP "format" $ \f -> when (f /= "RGBA8888") $ fail ("format must be RGBA8888, not " ++ f)
  _ <- varP "filter"
  _ <- varP "repeat"
  return (fileName, size)

vartP :: String -> (String -> Parser a) -> Parser a
vartP name f =
  ( do
      _ <- string (name ++ ":") >> spaces
      value <- manyTill anyChar endOfLine
      let inner = parse (f value) name value in either (fail . show) return inner
  )
    <?> ("var " ++ name)

varP :: String -> Parser String
varP name = vartP name return

{-
animation
  rotate: false
  xy: 2554, 379
  size: 520, 347
  orig: 520, 347
  offset: 0, 0
  index: -1
-}
regionP :: Parser (Key, G.UVec2, G.UVec2)
regionP = do
  name <- manyTill anyChar endOfLine
  _ <- spaces >> varP "rotate"
  xy <- spaces >> vartP "xy" (const uvec2P)
  size <- spaces >> vartP "size" (const uvec2P)
  _ <- spaces >> vartP "orig" (const uvec2P)
  _ <- spaces >> vartP "offset" (const uvec2P)
  index <- spaces >> vartP "index" (const $ optionMaybe word32P)
  return (Key (name, index), xy, size)

word32P :: Parser Word32
word32P = read <$> many1 digit

uvec2P :: Parser G.UVec2
uvec2P = do
  x <- word32P
  _ <- spaces >> char ',' >> spaces
  y <- word32P
  let v = G.uvec2 x y
  return v
