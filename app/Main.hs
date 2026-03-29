module Main (main) where

import Codec.Picture (readImage, savePngImage)
import PngDistanceMap.CLI
import PngDistanceMap.DistanceTransform
import PngDistanceMap.Image
import Data.Vector.Unboxed qualified as U
import System.Exit (die)

main :: IO ()
main = do
  cfg <- parseConfig
  decoded <- readImage (cfgInput cfg)
  dyn <- either die pure decoded
  source <- either die pure (dynamicToSourceImage dyn)

  let mask = buildMask cfg source
      w = sourceWidth source
      h = sourceHeight source
      unsignedField = unsignedDistanceField w h mask
      distances =
        case cfgMode cfg of
          UnsignedMode -> U.map sqrt (fieldDistancesSquared unsignedField)
          SignedMode -> signedDistanceMap w h mask
      outImage =
        renderOutput
          cfg
          source
          distances
          (fieldNearestX unsignedField)
          (fieldNearestY unsignedField)

  savePngImage (cfgOutput cfg) outImage
