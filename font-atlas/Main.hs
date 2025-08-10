{-# LANGUAGE RecordPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import Data.Functor (($>))
import Graphics.Text.PCF
import Utils (sayErr)

main :: IO ()
main = do
  let fontFile = "unifont-16.0.04.pcf"
  pcf <- either fail return =<< loadPCF fontFile
  PCFGlyph {glyph_width, glyph_height} <- maybe (sayErr "Font" "No such glyph") pure $ getPCFGlyph pcf 'A'
  empty <- JP.newMutableImage @JP.PixelRGBA8 glyph_width glyph_height
  image <- maybe (sayErr "Font" "No such glyph") (\glyph -> ashkan 0 0 glyph empty) $ getPCFGlyph pcf 'A'
  JP.writePng "font-atlas.png" =<< JP.freezeImage image
  where
    ashkan x0 y0 glyph image = foldPCFGlyphPixels glyph (\x y pix imgM -> do img <- imgM; JP.writePixel img (x0 + x) (y0 + y) (color pix) $> img) $ pure image
    color True = JP.PixelRGBA8 255 255 255 255
    color False = JP.PixelRGBA8 0 0 0 255
