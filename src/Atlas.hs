{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoFieldSelectors #-}

module Atlas
  ( atlas,
    Atlas (..),
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
import qualified Geomancy as G
import Measure
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

data Region = Region {region :: UVRegion, size :: PixelSize} deriving (Show)

newtype Regions = Regions (M.Map Key Region) deriving (Show)

mkRegion ::
  -- | Size of the atlas
  PixelSize ->
  -- | Position of the region
  PixelPosition ->
  -- | Size of the region
  PixelSize ->
  Region
mkRegion (PixelWH aw ah) (PixelXY rx ry) size@(PixelWH rw rh) =
  Region {region = uvReg (u rx) (v ry) (u $ rx + rw) (v $ ry + rh), size = size}
  where
    u x = fromIntegral x / fromIntegral aw
    v y = fromIntegral y / fromIntegral ah

lookup :: Regions -> String -> Region
lookup (Regions rs) name = rs M.! Key (name, Nothing)

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
headerP :: Parser (FilePath, PixelSize)
headerP = do
  _ <- endOfLine <?> "empty line"
  fileName <- manyTill anyChar endOfLine <?> "fileName"
  size <- vartP "size" (const pixelSizeP)
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
regionP :: Parser (Key, PixelPosition, PixelSize)
regionP = do
  name <- manyTill anyChar endOfLine
  _ <- spaces >> varP "rotate"
  xy <- spaces >> vartP "xy" (const pixelPositionP)
  size <- spaces >> vartP "size" (const pixelSizeP)
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

pixelSizeP :: Parser PixelSize
pixelSizeP = PixelSize <$> uvec2P

pixelPositionP :: Parser PixelPosition
pixelPositionP = PixelPosition <$> uvec2P

data Atlas = Atlas
  { texture :: Tex.DescriptorIndex,
    regions :: Regions
  }

-- TODO: find a way to reduce parameter count
withAtlas :: Vk.Allocator -> Vk.Device -> Vk.CommandPool -> Vk.Queue -> Vk.DescriptorSet -> Vk.Sampler -> String -> Managed Atlas
withAtlas allocator device commandPool gfxQueue descSet sampler atlasFile = do
  (textureFile, regions) <- either (sayErr "Atlas") return =<< runExceptT (atlas atlasFile)
  tex <- Tex.fromRGBA8PngFile allocator device commandPool gfxQueue $ "out/" ++ textureFile -- TODO:: remove "out/""
  [descIndex] <- Tex.bind device descSet [tex] sampler
  return $ Atlas {texture = descIndex, regions = regions}

sprite :: Atlas -> String -> PixelSize -> Tex.Sprite
sprite (Atlas {texture = tex, regions = atlas}) name windowSize =
  let reg = lookup atlas name
   in mkSprite tex reg windowSize

spriteIndexed :: Atlas -> String -> Word32 -> PixelSize -> Tex.Sprite
spriteIndexed (Atlas {texture = tex, regions = atlas}) name index windowSize =
  let reg = lookupIndexed atlas name index
   in mkSprite tex reg windowSize

mkSprite :: Tex.DescriptorIndex -> Region -> PixelSize -> Tex.Sprite
mkSprite tex Region {region = reg, size = size} windowSize =
  Tex.Sprite
    { texture = tex,
      region = reg,
      size = Measure.pixelSizeToNdc size windowSize
    }