module PngDistanceMap.CLI
  ( OutputBitDepth(..)
  , ChannelSource(..)
  , Config(..)
  , DistanceMode(..)
  , MaskChannel(..)
  , parseConfig
  ) where

import Options.Applicative
import Text.Read (readMaybe)


data OutputBitDepth
  = OutputBitDepth8
  | OutputBitDepth16
  deriving (Eq, Show)


data DistanceMode
  = UnsignedMode
  | SignedMode
  deriving (Eq, Show)


data MaskChannel
  = MaskAuto
  | MaskRed
  | MaskGreen
  | MaskBlue
  | MaskAlpha
  | MaskLuma
  deriving (Eq, Show)


data ChannelSource
  = ChannelDistance
  | ChannelNearestX
  | ChannelNearestY
  | ChannelRed
  | ChannelGreen
  | ChannelBlue
  | ChannelAlpha
  | ChannelLuma
  | ChannelZero
  | ChannelOne
  deriving (Eq, Show)


data Config = Config
  { cfgInput :: FilePath
  , cfgOutput :: FilePath
  , cfgOutputBitDepth :: OutputBitDepth
  , cfgScale :: Double
  , cfgMode :: DistanceMode
  , cfgMaskChannel :: MaskChannel
  , cfgThreshold :: Double
  , cfgInvertMask :: Bool
  , cfgOutputChannels :: [ChannelSource]
  }
  deriving (Eq, Show)

parseConfig :: IO Config
parseConfig = execParser parserInfo

parserInfo :: ParserInfo Config
parserInfo =
  info
    (configParser <**> helper)
    ( fullDesc
   <> progDesc "Generate distance maps and nearest-mask coordinates from PNG masks"
   <> header "png-distance-map"
    )

configParser :: Parser Config
configParser =
  Config
    <$> argument str
          ( metavar "INPUT.png"
         <> help "Input PNG file. Input bit depth is auto-detected by JuicyPixels."
          )
    <*> argument str
          ( metavar "OUTPUT.png"
         <> help "Output PNG file"
          )
    <*> option (eitherReader parseOutputBitDepth)
          ( long "output-bit-depth"
         <> long "bit-depth"
         <> metavar "8|16"
         <> value OutputBitDepth16
         <> showDefaultWith renderOutputBitDepth
         <> help "Output PNG sample depth. This does not control input decoding."
          )
    <*> option (eitherReader parsePositiveDouble)
          ( long "scale"
         <> metavar "FACTOR"
         <> value 1.0
         <> showDefault
         <> help "Encoded units per source pixel for distance channels"
          )
    <*> option (eitherReader parseDistanceMode)
          ( long "mode"
         <> metavar "unsigned|signed"
         <> value UnsignedMode
         <> showDefaultWith renderDistanceMode
         <> help "Whether d is an unsigned exterior distance or a signed distance"
          )
    <*> option (eitherReader parseMaskChannel)
          ( long "mask-channel"
         <> metavar "auto|r|g|b|a|l"
         <> value MaskAuto
         <> showDefaultWith renderMaskChannel
         <> help "Source channel used to define the mask"
          )
    <*> option (eitherReader parseUnitInterval)
          ( long "threshold"
         <> metavar "T"
         <> value 0.5
         <> showDefault
         <> help "Mask threshold in normalized units [0,1]"
          )
    <*> switch
          ( long "invert-mask"
         <> help "Invert the thresholded mask"
          )
    <*> option (eitherReader parseOutputChannels)
          ( long "output-channels"
         <> long "compose"
         <> metavar "SPEC"
         <> value [ChannelDistance]
         <> showDefaultWith renderOutputChannels
         <> help "Comma-separated output channel spec, e.g. d or x,y,d,a"
          )

parseOutputBitDepth :: String -> Either String OutputBitDepth
parseOutputBitDepth s =
  case s of
    "8" -> Right OutputBitDepth8
    "16" -> Right OutputBitDepth16
    _ -> Left "output bit depth must be 8 or 16"

parseDistanceMode :: String -> Either String DistanceMode
parseDistanceMode s =
  case s of
    "unsigned" -> Right UnsignedMode
    "signed" -> Right SignedMode
    _ -> Left "mode must be unsigned or signed"

parseMaskChannel :: String -> Either String MaskChannel
parseMaskChannel s =
  case s of
    "auto" -> Right MaskAuto
    "r" -> Right MaskRed
    "g" -> Right MaskGreen
    "b" -> Right MaskBlue
    "a" -> Right MaskAlpha
    "l" -> Right MaskLuma
    "y" -> Right MaskLuma
    _ -> Left "mask channel must be auto, r, g, b, a, or l"

parseOutputChannels :: String -> Either String [ChannelSource]
parseOutputChannels s =
  case traverse parseChannelSource (splitCommas s) of
    Left err -> Left err
    Right [] -> Left "output channel spec must contain between 1 and 4 channels"
    Right xs
      | length xs > 4 -> Left "output channel spec must contain between 1 and 4 channels"
      | otherwise -> Right xs

parseChannelSource :: String -> Either String ChannelSource
parseChannelSource s =
  case s of
    "d" -> Right ChannelDistance
    "x" -> Right ChannelNearestX
    "y" -> Right ChannelNearestY
    "r" -> Right ChannelRed
    "g" -> Right ChannelGreen
    "b" -> Right ChannelBlue
    "a" -> Right ChannelAlpha
    "l" -> Right ChannelLuma
    "0" -> Right ChannelZero
    "1" -> Right ChannelOne
    _ -> Left "output channels must be chosen from d,x,y,r,g,b,a,l,0,1"

parsePositiveDouble :: String -> Either String Double
parsePositiveDouble s =
  case readMaybe s of
    Just x | x > 0 -> Right x
    _ -> Left "expected a positive number"

parseUnitInterval :: String -> Either String Double
parseUnitInterval s =
  case readMaybe s of
    Just x | x >= 0 && x <= 1 -> Right x
    _ -> Left "expected a number in [0,1]"

renderOutputBitDepth :: OutputBitDepth -> String
renderOutputBitDepth bd =
  case bd of
    OutputBitDepth8 -> "8"
    OutputBitDepth16 -> "16"

renderDistanceMode :: DistanceMode -> String
renderDistanceMode m =
  case m of
    UnsignedMode -> "unsigned"
    SignedMode -> "signed"

renderMaskChannel :: MaskChannel -> String
renderMaskChannel mc =
  case mc of
    MaskAuto -> "auto"
    MaskRed -> "r"
    MaskGreen -> "g"
    MaskBlue -> "b"
    MaskAlpha -> "a"
    MaskLuma -> "l"

renderOutputChannels :: [ChannelSource] -> String
renderOutputChannels = concatMapWith "," renderChannelSource

renderChannelSource :: ChannelSource -> String
renderChannelSource ch =
  case ch of
    ChannelDistance -> "d"
    ChannelNearestX -> "x"
    ChannelNearestY -> "y"
    ChannelRed -> "r"
    ChannelGreen -> "g"
    ChannelBlue -> "b"
    ChannelAlpha -> "a"
    ChannelLuma -> "l"
    ChannelZero -> "0"
    ChannelOne -> "1"

splitCommas :: String -> [String]
splitCommas input =
  go input [] []
  where
    go [] current parts =
      let token = reverse current
      in reverse (if null token then parts else token : parts)
    go (',':xs) current parts =
      let token = reverse current
      in go xs [] (if null token then parts else token : parts)
    go (x:xs) current parts = go xs (x : current) parts

concatMapWith :: String -> (a -> String) -> [a] -> String
concatMapWith _ _ [] = ""
concatMapWith _ f [x] = f x
concatMapWith sep f (x:xs) = f x ++ sep ++ concatMapWith sep f xs
