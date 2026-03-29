module PngDistanceMap.Image
  ( SourceImage(..)
  , buildMask
  , dynamicToSourceImage
  , renderOutput
  ) where

import Codec.Picture
import Data.Vector.Unboxed qualified as U
import Data.Word (Word16, Word8)
import PngDistanceMap.CLI


data SourceImage = SourceImage
  { sourceWidth :: Int
  , sourceHeight :: Int
  , sourceHasAlpha :: Bool
  , sourcePixels :: Image PixelRGBA16
  }

dynamicToSourceImage :: DynamicImage -> Either String SourceImage
dynamicToSourceImage dyn =
  case dyn of
    ImageY8 img -> Right $ mkSource False (gray8ToRGBA16 img)
    ImageY16 img -> Right $ mkSource False (gray16ToRGBA16 img)
    ImageYA8 img -> Right $ mkSource True (grayAlpha8ToRGBA16 img)
    ImageYA16 img -> Right $ mkSource True (grayAlpha16ToRGBA16 img)
    ImageRGB8 img -> Right $ mkSource False (rgb8ToRGBA16 img)
    ImageRGB16 img -> Right $ mkSource False (rgb16ToRGBA16 img)
    ImageRGBA8 img -> Right $ mkSource True (rgba8ToRGBA16 img)
    ImageRGBA16 img -> Right $ mkSource True img
    _ -> Right $ mkSource True (rgba8ToRGBA16 (convertRGBA8 dyn))

buildMask :: Config -> SourceImage -> U.Vector Bool
buildMask cfg src =
  U.generate (w * h) $ \i ->
    let (row, col) = i `quotRem` w
        channelValue = maskChannelValue cfg src row col
        inside0 = channelValue >= cfgThreshold cfg
    in if cfgInvertMask cfg then not inside0 else inside0
  where
    w = sourceWidth src
    h = sourceHeight src

renderOutput :: Config -> SourceImage -> U.Vector Double -> U.Vector Int -> U.Vector Int -> DynamicImage
renderOutput cfg src distances nearestXs nearestYs =
  case (cfgOutputBitDepth cfg, cfgOutputChannels cfg) of
    (OutputBitDepth8, [c1]) ->
      ImageY8 $ generateImage (\x y -> resolve8 cfg src distances nearestXs nearestYs x y c1) w h
    (OutputBitDepth8, [c1, c2]) ->
      ImageYA8 $ generateImage (\x y -> PixelYA8 (resolve8 cfg src distances nearestXs nearestYs x y c1) (resolve8 cfg src distances nearestXs nearestYs x y c2)) w h
    (OutputBitDepth8, [c1, c2, c3]) ->
      ImageRGB8 $ generateImage (\x y -> PixelRGB8 (resolve8 cfg src distances nearestXs nearestYs x y c1) (resolve8 cfg src distances nearestXs nearestYs x y c2) (resolve8 cfg src distances nearestXs nearestYs x y c3)) w h
    (OutputBitDepth8, [c1, c2, c3, c4]) ->
      ImageRGBA8 $ generateImage (\x y -> PixelRGBA8 (resolve8 cfg src distances nearestXs nearestYs x y c1) (resolve8 cfg src distances nearestXs nearestYs x y c2) (resolve8 cfg src distances nearestXs nearestYs x y c3) (resolve8 cfg src distances nearestXs nearestYs x y c4)) w h
    (OutputBitDepth16, [c1]) ->
      ImageY16 $ generateImage (\x y -> resolve16 cfg src distances nearestXs nearestYs x y c1) w h
    (OutputBitDepth16, [c1, c2]) ->
      ImageYA16 $ generateImage (\x y -> PixelYA16 (resolve16 cfg src distances nearestXs nearestYs x y c1) (resolve16 cfg src distances nearestXs nearestYs x y c2)) w h
    (OutputBitDepth16, [c1, c2, c3]) ->
      ImageRGB16 $ generateImage (\x y -> PixelRGB16 (resolve16 cfg src distances nearestXs nearestYs x y c1) (resolve16 cfg src distances nearestXs nearestYs x y c2) (resolve16 cfg src distances nearestXs nearestYs x y c3)) w h
    (OutputBitDepth16, [c1, c2, c3, c4]) ->
      ImageRGBA16 $ generateImage (\x y -> PixelRGBA16 (resolve16 cfg src distances nearestXs nearestYs x y c1) (resolve16 cfg src distances nearestXs nearestYs x y c2) (resolve16 cfg src distances nearestXs nearestYs x y c3) (resolve16 cfg src distances nearestXs nearestYs x y c4)) w h
    (_, []) -> error "output channels must contain between 1 and 4 channels"
    (_, _) -> error "output channels must contain between 1 and 4 channels"
  where
    w = sourceWidth src
    h = sourceHeight src

mkSource :: Bool -> Image PixelRGBA16 -> SourceImage
mkSource hasAlpha img =
  SourceImage
    { sourceWidth = imageWidth img
    , sourceHeight = imageHeight img
    , sourceHasAlpha = hasAlpha
    , sourcePixels = img
    }

gray8ToRGBA16 :: Image Pixel8 -> Image PixelRGBA16
gray8ToRGBA16 img =
  generateImage
    (\x y ->
      let g = up8 (pixelAt img x y)
      in PixelRGBA16 g g g maxBound)
    (imageWidth img)
    (imageHeight img)

gray16ToRGBA16 :: Image Pixel16 -> Image PixelRGBA16
gray16ToRGBA16 img =
  generateImage
    (\x y ->
      let g = pixelAt img x y
      in PixelRGBA16 g g g maxBound)
    (imageWidth img)
    (imageHeight img)

grayAlpha8ToRGBA16 :: Image PixelYA8 -> Image PixelRGBA16
grayAlpha8ToRGBA16 img =
  generateImage
    (\x y ->
      let PixelYA8 g a = pixelAt img x y
          g16 = up8 g
          a16 = up8 a
      in PixelRGBA16 g16 g16 g16 a16)
    (imageWidth img)
    (imageHeight img)

grayAlpha16ToRGBA16 :: Image PixelYA16 -> Image PixelRGBA16
grayAlpha16ToRGBA16 img =
  generateImage
    (\x y ->
      let PixelYA16 g a = pixelAt img x y
      in PixelRGBA16 g g g a)
    (imageWidth img)
    (imageHeight img)

rgb8ToRGBA16 :: Image PixelRGB8 -> Image PixelRGBA16
rgb8ToRGBA16 img =
  generateImage
    (\x y ->
      let PixelRGB8 r g b = pixelAt img x y
      in PixelRGBA16 (up8 r) (up8 g) (up8 b) maxBound)
    (imageWidth img)
    (imageHeight img)

rgb16ToRGBA16 :: Image PixelRGB16 -> Image PixelRGBA16
rgb16ToRGBA16 img =
  generateImage
    (\x y ->
      let PixelRGB16 r g b = pixelAt img x y
      in PixelRGBA16 r g b maxBound)
    (imageWidth img)
    (imageHeight img)

rgba8ToRGBA16 :: Image PixelRGBA8 -> Image PixelRGBA16
rgba8ToRGBA16 img =
  generateImage
    (\x y ->
      let PixelRGBA8 r g b a = pixelAt img x y
      in PixelRGBA16 (up8 r) (up8 g) (up8 b) (up8 a))
    (imageWidth img)
    (imageHeight img)

up8 :: Word8 -> Word16
up8 x = fromIntegral x * 257

maskChannelValue :: Config -> SourceImage -> Int -> Int -> Double
maskChannelValue cfg src y x =
  case effectiveMaskChannel cfg src of
    MaskAuto -> channelAsDouble ChannelLuma src x y
    MaskRed -> channelAsDouble ChannelRed src x y
    MaskGreen -> channelAsDouble ChannelGreen src x y
    MaskBlue -> channelAsDouble ChannelBlue src x y
    MaskAlpha -> channelAsDouble ChannelAlpha src x y
    MaskLuma -> channelAsDouble ChannelLuma src x y

effectiveMaskChannel :: Config -> SourceImage -> MaskChannel
effectiveMaskChannel cfg src =
  case cfgMaskChannel cfg of
    MaskAuto -> if sourceHasAlpha src then MaskAlpha else MaskLuma
    other -> other

resolve8 :: Config -> SourceImage -> U.Vector Double -> U.Vector Int -> U.Vector Int -> Int -> Int -> ChannelSource -> Word8
resolve8 cfg src distances nearestXs nearestYs x y ch =
  case ch of
    ChannelDistance -> encodeDistance8 (cfgMode cfg) (cfgScale cfg) distance
    ChannelNearestX -> encodeCoord8 (sourceWidth src) nearestX
    ChannelNearestY -> encodeCoord8 (sourceHeight src) nearestY
    ChannelRed -> down16To8 r
    ChannelGreen -> down16To8 g
    ChannelBlue -> down16To8 b
    ChannelAlpha -> down16To8 a
    ChannelLuma -> down16To8 (luma16FromPixel pixel)
    ChannelZero -> 0
    ChannelOne -> maxBound
  where
    pixel@(PixelRGBA16 r g b a) = pixelAt (sourcePixels src) x y
    i = y * sourceWidth src + x
    distance = distances U.! i
    nearestX = nearestXs U.! i
    nearestY = nearestYs U.! i

resolve16 :: Config -> SourceImage -> U.Vector Double -> U.Vector Int -> U.Vector Int -> Int -> Int -> ChannelSource -> Word16
resolve16 cfg src distances nearestXs nearestYs x y ch =
  case ch of
    ChannelDistance -> encodeDistance16 (cfgMode cfg) (cfgScale cfg) distance
    ChannelNearestX -> encodeCoord16 (sourceWidth src) nearestX
    ChannelNearestY -> encodeCoord16 (sourceHeight src) nearestY
    ChannelRed -> r
    ChannelGreen -> g
    ChannelBlue -> b
    ChannelAlpha -> a
    ChannelLuma -> luma16FromPixel pixel
    ChannelZero -> 0
    ChannelOne -> maxBound
  where
    pixel@(PixelRGBA16 r g b a) = pixelAt (sourcePixels src) x y
    i = y * sourceWidth src + x
    distance = distances U.! i
    nearestX = nearestXs U.! i
    nearestY = nearestYs U.! i

channelAsDouble :: ChannelSource -> SourceImage -> Int -> Int -> Double
channelAsDouble ch src x y =
  fromIntegral (channelAsWord16 ch src x y) / fromIntegral (maxBound :: Word16)

channelAsWord16 :: ChannelSource -> SourceImage -> Int -> Int -> Word16
channelAsWord16 ch src x y =
  case ch of
    ChannelRed -> r
    ChannelGreen -> g
    ChannelBlue -> b
    ChannelAlpha -> a
    ChannelLuma -> luma16FromPixel pixel
    ChannelZero -> 0
    ChannelOne -> maxBound
    ChannelDistance -> error "distance is not an input channel"
    ChannelNearestX -> error "nearest-x is not an input channel"
    ChannelNearestY -> error "nearest-y is not an input channel"
  where
    pixel@(PixelRGBA16 r g b a) = pixelAt (sourcePixels src) x y

luma16FromPixel :: PixelRGBA16 -> Word16
luma16FromPixel (PixelRGBA16 r g b _) =
  clampWord16 (0.2126 * fromIntegral r + 0.7152 * fromIntegral g + 0.0722 * fromIntegral b)

encodeDistance8 :: DistanceMode -> Double -> Double -> Word8
encodeDistance8 mode scale distance =
  clampWord8 raw
  where
    raw =
      case mode of
        UnsignedMode -> distance * scale
        SignedMode -> midpoint + distance * scale
    midpoint = fromIntegral (maxBound :: Word8) / 2

encodeDistance16 :: DistanceMode -> Double -> Double -> Word16
encodeDistance16 mode scale distance =
  clampWord16 raw
  where
    raw =
      case mode of
        UnsignedMode -> distance * scale
        SignedMode -> midpoint + distance * scale
    midpoint = fromIntegral (maxBound :: Word16) / 2

encodeCoord8 :: Int -> Int -> Word8
encodeCoord8 extent coord =
  clampWord8 (coordToSample extent coord (maxBound :: Word8))

encodeCoord16 :: Int -> Int -> Word16
encodeCoord16 extent coord =
  clampWord16 (coordToSample extent coord (maxBound :: Word16))

coordToSample :: (Integral a) => Int -> Int -> a -> Double
coordToSample extent coord sampleMax
  | coord < 0 = 0
  | extent <= 1 = 0
  | otherwise =
      fromIntegral coord * fromIntegral sampleMax / fromIntegral (extent - 1)

clampWord8 :: Double -> Word8
clampWord8 x
  | isNaN x = 0
  | isInfinite x && x > 0 = maxBound
  | isInfinite x = 0
  | x <= 0 = 0
  | x >= fromIntegral (maxBound :: Word8) = maxBound
  | otherwise = round x

clampWord16 :: Double -> Word16
clampWord16 x
  | isNaN x = 0
  | isInfinite x && x > 0 = maxBound
  | isInfinite x = 0
  | x <= 0 = 0
  | x >= fromIntegral (maxBound :: Word16) = maxBound
  | otherwise = round x

down16To8 :: Word16 -> Word8
down16To8 w = fromIntegral (((fromIntegral w :: Int) + 128) `quot` 257)
