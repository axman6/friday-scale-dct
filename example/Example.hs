module Main where

import           Codec.Picture                   (DynamicImage (..), readImage,
                                                  writePng)
import           Codec.Picture.RGBA8             (fromDynamicImage)
import           Data.Time                       (diffUTCTime, getCurrentTime)
import           Vision.Image.Transform.ScaleDCT (scale)

import           Vision.Image.JuicyPixels

main :: IO ()
main = do
    start <- getCurrentTime
    Right dimg <- readImage "phadej.png"
    let img = fromDynamicImage dimg
    let avasmall = scale (64, 64) (toFridayRGBA img)
    let avalarge = scale (600, 600) (toFridayRGBA img)
    let avahuge  = scale (1200, 1200) (toFridayRGBA img)
    writePng "bio-small.png" (toJuicyRGBA avasmall)
    writePng "bio-large.png" (toJuicyRGBA avalarge)
    writePng "bio-huge.png"  (toJuicyRGBA avahuge)
    end <- getCurrentTime
    print $ end `diffUTCTime` start


showType :: DynamicImage  -> String
showType dimg = case dimg of
    ImageY8 _ -> "ImageY8"
    ImageY16 _ -> "ImageY16"
    ImageYF _ -> "ImageYF"
    ImageYA8 _ -> "ImageYA8"
    ImageYA16 _ -> "ImageYA16"
    ImageRGB8 _ -> "ImageRGB8"
    ImageRGB16 _ -> "ImageRGB16"
    ImageRGBF _ -> "ImageRGBF"
    ImageRGBA8 _ -> "ImageRGBA8"
    ImageRGBA16 _ -> "ImageRGBA16"
    ImageYCbCr8 _ -> "ImageYCbCr8"
    ImageCMYK8 _ -> "ImageCMYK8"
    ImageCMYK16 _ -> "ImageCMYK16"
