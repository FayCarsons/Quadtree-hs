{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad
import qualified Control.Monad.State as State
import GHC.Float (pi)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Point as P
import qualified Shape as S
import qualified System.Random as Random
import qualified Tree as QT
import Prelude

{- Constants -}
maxLeafPoints :: Int
maxLeafPoints = 4

numClusters :: Int
numClusters = 12

pointsPerCluster :: Int
pointsPerCluster = 96

spread :: Double
spread = 0.075

resolution :: (Int, Int)
resolution = (1600, 1600)

outputFileName :: String
outputFileName = "quadtree.png"

{- Random point generation -}

type RNGState = State.State Random.StdGen

generateCluster :: P.Point Double -> Int -> Double -> RNGState [P.Point Double]
generateCluster (P.Point cx cy) n spread = Control.Monad.replicateM n randomClusterPoint
  where
    randomClusterPoint = do
      dx <- State.state $ Random.randomR (-spread, spread)
      dy <- State.state $ Random.randomR (-spread, spread)
      return $ P.Point (cx + dx) (cy + dy)

generateClusters :: Int -> Int -> Double -> RNGState [[P.Point Double]]
generateClusters numClusters pointsPerCluster spread = do
  centers <- Control.Monad.replicateM numClusters (State.state Random.random)
  mapM (\center -> generateCluster center pointsPerCluster spread) centers

{- Rendering -}
draw :: S.Shape -> Cairo.Render ()
draw (S.Rectangle (P.Point x y) width height) =
  do
    Cairo.setSourceRGBA 0 0 0 1
    Cairo.rectangle x y width height
    Cairo.stroke
draw (S.Circle (P.Point x y) radius) =
  do
    Cairo.setSourceRGBA 1 0.01 0.01 1
    Cairo.arc x y radius 0.0 (2 * realToFrac pi)
    Cairo.strokePreserve
    Cairo.fill

main :: IO ()
main = do
  gen <- Random.getStdGen
  let clusters = State.evalState (generateClusters numClusters pointsPerCluster spread) gen
      points = map scale $ concat clusters
      shapes = S.toFlatShapes $ QT.build resolution maxLeafPoints points

  surface <- uncurry (Cairo.createImageSurface Cairo.FORMAT_RGBA128F) resolution
  Cairo.renderWith surface $ do
    Cairo.setSourceRGB 1 1 1
    Cairo.paint
    mapM_ draw shapes
  Cairo.surfaceWriteToPNG surface outputFileName
  where
    scale (P.Point x y) = P.Point (realToFrac $ x * fromIntegral (fst resolution)) (realToFrac $ y * fromIntegral (snd resolution))
