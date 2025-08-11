{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import Data.Char (chr)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Graphics.Text.PCF
import Text.Printf (printf)
import Utils (sayErr)

{--
atlas.png
size: 600, 510
format: RGBA8888
filter: Nearest, Nearest
repeat: none
1
  rotate: false
  xy: 0, 300
  size: 100, 100
  orig: 100, 100
  offset: 0, 0
  index: -1
-}
main :: IO ()
main = do
  let fontFile = "unifont-16.0.04.pcf"
      min = 0x0020
      max = 0x007E
  pcf <- either err return =<< loadPCF fontFile
  PCFGlyph {glyph_width, glyph_height} <- glyph pcf 'A'
  atlas <- JP.newMutableImage @JP.PixelRGBA8 (glyph_width * (max - min + 1)) glyph_height
  (width, regions, image2) <-
    foldl'
      ( \acc ch ->
          do
            gl <- glyph pcf (chr ch)
            (x0, regions, atlas) <- acc
            atlas' <- ashkan x0 0 gl atlas
            return (x0 + gl.glyph_width, (ch, gl.glyph_width, gl.glyph_height, x0, 0 :: Int) : regions, atlas')
      )
      (pure (0, [], atlas))
      [min .. max]
  JP.writePng "font.png" =<< JP.freezeImage image2
  let header =
        [ "",
          "font.png",
          "size: " ++ show width ++ "," ++ " " ++ show glyph_height,
          "format: RGBA8888",
          "filter: Nearest, Nearest",
          "repeat: none"
        ]
      rs =
        ( \(c, w, h, x, y) ->
            [ printf "U+%04X" c,
              "  rotate: false",
              "  xy: " ++ show x ++ ", " ++ show y,
              "  size: " ++ show w ++ ", " ++ show h,
              "  orig: 0, 0",
              "  offset: 0, 0",
              "  index: -1"
            ]
        )
          =<< regions
  writeFile "font.atlas" $ mconcat ((++ "\n") <$> (header ++ rs))
  where
    ashkan x0 y0 glyph image = foldPCFGlyphPixels glyph (\x y pix imgM -> do img <- imgM; JP.writePixel img (x0 + x) (y0 + y) (color pix) $> img) $ pure image
    color b = JP.PixelRGBA8 255 255 255 $ if b then 255 else 0
    glyph pcf g = maybe (err $ "No such glyph" ++ [g]) pure $ getPCFGlyph pcf g
    err = sayErr "Font"
