{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoFieldSelectors #-}

module Atlas
  (
    Atlas (..),
    Sprite (..),
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

newtype Regions = Regions (M.Map Key TextureRegion) deriving (Show)

mkRegion ::
  -- | Size of the atlas
  PixelSize ->
  -- | Position of the region
  PixelPosition ->
  -- | Size of the region
  PixelSize ->
  TextureRegion
mkRegion (PixelWH sx sy) (PixelXY x y) (PixelWH w h) =
  TextureRegion $ G.vec4 (u x) (v y) (u $ x + w) (v $ y + h)
  where
    u x = fromIntegral x / fromIntegral sx
    v y = fromIntegral y / fromIntegral sy

lookup :: Regions -> String -> TextureRegion
lookup (Regions rs) name = rs M.! Key (name, Nothing)

lookupIndexed :: Regions -> String -> Word32 -> TextureRegion
lookupIndexed (Regions rs) name index = rs M.! Key (name, Just index)

atlas :: (MonadError String m, MonadIO m) => FilePath -> m (FilePath, Regions)
atlas file = liftIO (parseFromFile atlasP file) >>= either (throwError . show) return

atlasP :: Parser (FilePath, Regions)
atlasP = do
  (fileName, s) <- headerP <?> "header"
  regions <- manyTill (regionP <&> \(k, xy, size) -> (k, mkRegion s xy size)) eof
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
  { texture :: Tex.BoundTexture,
    atlas :: Regions
  }

withAtlas :: Vk.Allocator -> Vk.Device -> Vk.CommandPool -> Vk.Queue -> Vk.DescriptorSet -> Vk.Sampler -> String -> Managed Atlas
withAtlas allocator device commandPool gfxQueue descSet sampler atlasFile = do
  (atlasTextureFile, atlas) <- either (sayErr "Atlas") return =<< runExceptT (atlas atlasFile)
  atlasTexture <- Tex.texture allocator device commandPool gfxQueue $ "out/" ++ atlasTextureFile -- TODO:: remove "out/""
  [boundAtlasTexture] <- Tex.bindTextures device descSet [atlasTexture] sampler
  return $ Atlas { texture = boundAtlasTexture, atlas = atlas}

data Sprite = Sprite
  { texture :: Tex.BoundTexture,
    region :: Measure.TextureRegion
  }

sprite :: Atlas -> String -> Sprite
sprite (Atlas {texture = tex, atlas = atlas}) name = Sprite {texture = tex, region = lookup atlas name}

spriteIndexed :: Atlas -> String -> Word32 -> Sprite
spriteIndexed (Atlas {texture = tex, atlas = atlas}) name index =
  Sprite
    { texture = tex,
      region = lookupIndexed atlas name index
    }