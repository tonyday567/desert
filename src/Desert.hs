{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Desert where

import Chart hiding (toFloat)
import NumHask.Prelude as P
import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V
import Control.Lens

-- | Turn a PNG into a [Chart Double]
--
-- > (Just i) <- getImage
-- > writeFile "other/scratch.svg" $ renderHudOptionsChart defaultSvgOptions defaultHudOptions [] (toChart i)

getImage :: IO (Image PixelRGB8)
getImage = do
  bs <- B.readFile "other/garfield.png"
  pure $ either (const empty') extracti $ decodePng bs
    where
      extracti x = case x of
                     (ImageRGB8 i) -> i
                     _ -> empty'
      empty' = Image 0 0 V.empty

toColour :: PixelRGB8 -> Colour
toColour = (\(PixelRGB8 r g b) -> Colour (fromIntegral r/256) (fromIntegral g/256) (fromIntegral b/256) 1)

--  Rectangle chart made of single rectangles of colour.
fillColor :: [[Colour]] -> [Chart Double]
fillColor css =
  zipWith
  (\c (x', y') ->
      Chart (RectA (RectStyle 0 transparent c))
      [RectXY (Rect (fromIntegral x') (fromIntegral x'+1) (fromIntegral y') (fromIntegral y' + 1))])
  (mconcat css)
  ((,) <$>
   [0..height - 1::Int] <*>
   [0..width' - 1::Int])
  where
    height = length css
    width' = maybe 0 length (head css)

-- | reverses the top to bottom convention
crop :: Rect Int -> [[Colour]] -> [[Colour]]
crop (Rect x z y w) css = fmap (take (w-y)) . take (z - x) . fmap (drop y) . drop x $ css

go :: IO ()
go = do
  i <- getImage
  writeFile "other/scratch.svg" $
    renderHudOptionsChart
    (defaultSvgOptions &
     #svgHeight .~ fromIntegral h &
     #svgAspect .~ ManualAspect asp)
    defaultHudOptions []
    (fillColor $ crop r $ fromImage i)
    where
      r@(Rect x z y w) = Rect (0::Int) 300 0 200
      h = w - y
      w' = z - x
      asp = fromIntegral w' / fromIntegral h

fromImage :: Image PixelRGB8 -> [[Colour]]
fromImage (Image w h v) =
  -- from top to bottom
  generate h (\y ->
    generate w (\x -> let i = (x + (h - y) * w) * 3 in
                   Colour
                   (toFloat $ v V.! i)
                   (toFloat $ v V.! (i+1))
                   (toFloat $ v V.! (i+2))
                   1))
  where
    toFloat x = fromIntegral x / 256.0

generate :: Int -> (Int -> a) -> [a]
generate n f = f <$> (take n [0..])

mult :: Rect Double -> [Chart Double] -> [Chart Double]
mult r cs = (\(Chart a xys') -> Chart a ((RectXY . (r *) . toRect) <$> xys')) <$> cs
