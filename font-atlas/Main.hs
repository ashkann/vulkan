{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import Data.Foldable (foldl')
import Data.Functor (($>))
import Graphics.Text.PCF
import Utils (sayErr)
import Data.Char (chr)

main :: IO ()
main = do
  let fontFile = "unifont-16.0.04.pcf"
      min = 0x0020 
      max = 0x007E
  pcf <- either err return =<< loadPCF fontFile
  PCFGlyph {glyph_width, glyph_height} <- glyph pcf 'A'
  atlas <- JP.newMutableImage @JP.PixelRGBA8 (glyph_width * (max - min + 1)) glyph_height
  (_, image2) <-
    foldl'
      ( \acc ch ->
          do
            gl <- glyph pcf (chr ch)
            (x0, atlas) <- acc
            atlas' <- ashkan x0 0 gl atlas
            return (x0 + gl.glyph_width, atlas')
      )
      (pure (0, atlas))
      [min .. max]
  JP.writePng "font-atlas.png" =<< JP.freezeImage image2
  where
    ashkan x0 y0 glyph image = foldPCFGlyphPixels glyph (\x y pix imgM -> do img <- imgM; JP.writePixel img (x0 + x) (y0 + y) (color pix) $> img) $ pure image
    color b = JP.PixelRGBA8 255 255 255 $ if b then 255 else 0
    glyph pcf g = maybe (err $ "No such glyph" ++ [g]) pure $ getPCFGlyph pcf g
    err = sayErr "Font"
