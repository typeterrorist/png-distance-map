module PngDistanceMap.DistanceTransform
  ( DistanceField(..)
  , signedDistanceMap
  , unsignedDistanceField
  , unsignedDistanceMap
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM


data DistanceField = DistanceField
  { fieldDistancesSquared :: U.Vector Double
  , fieldNearestX :: U.Vector Int
  , fieldNearestY :: U.Vector Int
  }

unsignedDistanceField :: Int -> Int -> U.Vector Bool -> DistanceField
unsignedDistanceField w h seeds = edtSquaredWithNearest w h seeds

unsignedDistanceMap :: Int -> Int -> U.Vector Bool -> U.Vector Double
unsignedDistanceMap w h seeds =
  U.map sqrt (fieldDistancesSquared (unsignedDistanceField w h seeds))

signedDistanceMap :: Int -> Int -> U.Vector Bool -> U.Vector Double
signedDistanceMap w h mask =
  U.zipWith (-)
    (unsignedDistanceMap w h mask)
    (unsignedDistanceMap w h (U.map not mask))

edtSquaredWithNearest :: Int -> Int -> U.Vector Bool -> DistanceField
edtSquaredWithNearest w h seeds =
  let inf = 1 / 0
      base = U.generate (w * h) $ \i -> if seeds U.! i then 0 else inf
      rowPass = runST $ do
        outDist <- UM.new (w * h)
        outArgX <- UM.new (w * h)
        forM_ [0 .. h - 1] $ \y -> do
          let row = U.generate w $ \x -> base U.! indexOf w x y
              (rowDist, rowArg) = distanceTransform1D row
          forM_ [0 .. w - 1] $ \x -> do
            UM.write outDist (indexOf w x y) (rowDist U.! x)
            UM.write outArgX (indexOf w x y) (rowArg U.! x)
        distFrozen <- U.unsafeFreeze outDist
        argFrozen <- U.unsafeFreeze outArgX
        pure (distFrozen, argFrozen)
  in runST $ do
       outDist <- UM.new (w * h)
       outNearestX <- UM.new (w * h)
       outNearestY <- UM.new (w * h)
       forM_ [0 .. w - 1] $ \x -> do
         let col = U.generate h $ \y -> fst rowPass U.! indexOf w x y
             (colDist, colArgY) = distanceTransform1D col
         forM_ [0 .. h - 1] $ \y -> do
           let i = indexOf w x y
               nearestRow = colArgY U.! y
               nearestX = if nearestRow < 0 then -1 else snd rowPass U.! indexOf w x nearestRow
               nearestY = nearestRow
           UM.write outDist i (colDist U.! y)
           UM.write outNearestX i nearestX
           UM.write outNearestY i nearestY
       distFrozen <- U.unsafeFreeze outDist
       xFrozen <- U.unsafeFreeze outNearestX
       yFrozen <- U.unsafeFreeze outNearestY
       pure DistanceField
         { fieldDistancesSquared = distFrozen
         , fieldNearestX = xFrozen
         , fieldNearestY = yFrozen
         }

distanceTransform1D :: U.Vector Double -> (U.Vector Double, U.Vector Int)
distanceTransform1D f =
  case firstFiniteIndex f of
    Nothing -> (U.replicate n inf, U.replicate n (-1))
    Just p0 -> runST $ do
      v <- UM.new n
      z <- UM.new (n + 1)
      d <- UM.new n
      arg <- UM.new n

      UM.write v 0 p0
      UM.write z 0 negInf
      UM.write z 1 inf

      kMax <- insertParabolas v z 0 (p0 + 1)
      evaluateEnvelope v z d arg kMax 0 0
      dFrozen <- U.unsafeFreeze d
      argFrozen <- U.unsafeFreeze arg
      pure (dFrozen, argFrozen)
  where
    n = U.length f
    inf = 1 / 0
    negInf = -1 / 0

    insertParabolas
      :: UM.MVector s Int
      -> UM.MVector s Double
      -> Int
      -> Int
      -> ST s Int
    insertParabolas v z !k !q
      | q >= n = pure k
      | isInfiniteValue (f U.! q) = insertParabolas v z k (q + 1)
      | otherwise = do
          k' <- pushParabola v z k q
          insertParabolas v z k' (q + 1)

    pushParabola
      :: UM.MVector s Int
      -> UM.MVector s Double
      -> Int
      -> Int
      -> ST s Int
    pushParabola v z !k !q = do
      p <- UM.read v k
      let s = intersection f q p
      zK <- UM.read z k
      if s <= zK
        then pushParabola v z (k - 1) q
        else do
          let k' = k + 1
          UM.write v k' q
          UM.write z k' s
          UM.write z (k' + 1) inf
          pure k'

    evaluateEnvelope
      :: UM.MVector s Int
      -> UM.MVector s Double
      -> UM.MVector s Double
      -> UM.MVector s Int
      -> Int
      -> Int
      -> Int
      -> ST s ()
    evaluateEnvelope v z d arg !kMax !k !q
      | q >= n = pure ()
      | otherwise = do
          nextBoundary <- UM.read z (k + 1)
          if k < kMax && nextBoundary < fromIntegral q
            then evaluateEnvelope v z d arg kMax (k + 1) q
            else do
              p <- UM.read v k
              let qd = fromIntegral q :: Double
                  pd = fromIntegral p :: Double
                  value = sqr (qd - pd) + f U.! p
              UM.write d q value
              UM.write arg q p
              evaluateEnvelope v z d arg kMax k (q + 1)

firstFiniteIndex :: U.Vector Double -> Maybe Int
firstFiniteIndex = U.findIndex (not . isInfiniteValue)

isInfiniteValue :: Double -> Bool
isInfiniteValue = isInfinite

intersection :: U.Vector Double -> Int -> Int -> Double
intersection f q p =
  ((f U.! q + sqr qd) - (f U.! p + sqr pd)) / (2 * (qd - pd))
  where
    qd = fromIntegral q :: Double
    pd = fromIntegral p :: Double

indexOf :: Int -> Int -> Int -> Int
indexOf w x y = y * w + x

sqr :: Num a => a -> a
sqr x = x * x
