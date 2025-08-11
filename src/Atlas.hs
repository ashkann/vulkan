{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Atlas
  ( atlas,
    Atlas,
    withAtlas,
    sprite,
    spriteIndexed,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (Managed)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Word (Word32)
import Measure
import Sprite (Sprite (..))
import System.FilePath (replaceFileName)
import Text.Parsec (anyChar, char, digit, endOfLine, eof, many1, manyTill, optionMaybe, parse, string, (<?>))
import Text.Parsec.Char (spaces)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Texture as Tex
import Utils (sayErr)
import qualified Vulkan as Vk
import qualified VulkanMemoryAllocator as Vk
import Prelude hiding (lookup)

newtype Key = Key (String, Maybe Word32)
  deriving (Show)
  deriving (Ord, Eq)

data Region = Region {region :: UVRegion, size :: PixelVec} deriving (Show)

newtype Regions = Regions (M.Map Key Region) deriving (Show)

mkRegion ::
  -- | Size of the atlas
  PixelVec ->
  -- | Position of the region
  PixelVec ->
  -- | Size of the region
  PixelVec ->
  Region
mkRegion (WithVec aw ah) (WithVec rx ry) size@(WithVec rw rh) =
  Region {region = uvReg (u rx) (v ry) (u $ rx + rw) (v $ ry + rh), size = size}
  where
    u x = x / aw
    v y = y / ah

lookup :: Regions -> String -> Maybe Region
lookup (Regions rs) name = let key = Key (name, Nothing) in M.lookup key rs

lookupIndexed :: Regions -> String -> Word32 -> Region
lookupIndexed (Regions rs) name index = rs M.! Key (name, Just index)

atlas :: (MonadError String m, MonadIO m) => FilePath -> m (FilePath, Regions)
atlas file = liftIO (parseFromFile atlasP file) >>= either (throwError . show) return

atlasP :: Parser (FilePath, Regions)
atlasP = do
  (fileName, atlasSize) <- headerP <?> "header"
  regions <- manyTill (regionP <&> \(k, pos, size) -> (k, mkRegion atlasSize pos size)) eof
  return (fileName, Regions $ M.fromList regions)

-- | Parse the atlas header
--
-- @
-- (empty line)
-- atlas.png
-- size: 3165, 2052
-- format: RGBA8888
-- filter: Nearest, Nearest
-- repeat: none
-- @
headerP :: Parser (FilePath, PixelVec)
headerP = do
  _ <- endOfLine <?> "empty line"
  fileName <- manyTill anyChar endOfLine <?> "fileName"
  size <- vartP "size" (const vec2P)
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

-- | Parse a region
--
-- @
-- animation
--  rotate: false
--  xy: 2554, 379
--  size: 520, 347
--  orig: 520, 347
--  offset: 0, 0
--  index: -1
-- @
regionP :: Parser (Key, PixelVec, PixelVec)
regionP = do
  name <- manyTill anyChar endOfLine
  _ <- spaces >> varP "rotate"
  xy <- spaces >> vartP "xy" (const vec2P)
  size <- spaces >> vartP "size" (const vec2P)
  _ <- spaces >> vartP "orig" (const vec2P)
  _ <- spaces >> vartP "offset" (const vec2P)
  index <- spaces >> vartP "index" (const $ optionMaybe word32P)
  return (Key (name, index), xy, size)

word32P :: Parser Word32
word32P = read <$> many1 digit

vec2P :: Parser PixelVec
vec2P = do
  x <- word32P
  _ <- spaces >> char ',' >> spaces
  y <- word32P
  let v = vec (fromIntegral x) (fromIntegral y)
  return v

data Atlas = Atlas
  { texture :: Tex.DescriptorIndex,
    regions :: Regions
  }

-- TODO: find a way to reduce parameter count
withAtlas :: Vk.Allocator -> Vk.Device -> Vk.CommandPool -> Vk.Queue -> Vk.DescriptorSet -> Vk.Sampler -> [FilePath] -> Managed [Atlas]
withAtlas allocator device cmdPool gfx set sampler atlasFiles = do
  (texs, regions) <- unzip <$> traverse (\x -> do (a, b) <- parse x; view <- f x a; return (view, b)) atlasFiles
  idxs <- Tex.bind device set texs sampler
  return $ zipWith Atlas idxs regions
  where
    parse f = either (sayErr "Atlas") return =<< runExceptT (atlas f)
    f atlasFilePath imageFileName = Tex.fromPngRGBA8File allocator device cmdPool gfx $ replaceFileName atlasFilePath imageFileName

sprite :: Atlas -> String -> Sprite
sprite atlas name = lookupOrFail (mkSprite atlas.texture)
  where
    lookupOrFail f = maybe (notFound name) f (lookup atlas.regions name)
    notFound name = error $ "Can't find region " ++ name ++ " in atlas"

spriteIndexed :: Atlas -> String -> Word32 -> Sprite
spriteIndexed (Atlas {texture = tex, regions = atlas}) name index =
  let reg = lookupIndexed atlas name index
   in mkSprite tex reg 

mkSprite :: Tex.DescriptorIndex -> Region -> Sprite
mkSprite tex Region {region = reg, size = res} =
  Sprite
    { texture = tex,
      region = reg,
      resolution = res,
      origin = vec 0 0
    }