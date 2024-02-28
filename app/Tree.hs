module Tree
  ( Quadtree (..),
    build,
    Box (..),
    midpoint,
  )
where

import Point (Point (..), splat, (+~), (/!))

{- Box type & utilities -}
data Box = Box
  { min :: Point Double,
    max :: Point Double
  }

midpoint :: Box -> Point Double
midpoint (Box min max) = min +~ max /! 2.0

quarter :: Box -> (Box, Box, Box, Box)
quarter box@(Box min max) =
  (lu, ru, rd, ld)
  where
    mid = midpoint box
    lu = Box (Point (x min) (y mid)) (Point (x mid) (y max))
    ru = Box mid max
    rd = Box (Point (x mid) (y min)) (Point (x max) (y mid))
    ld = Box min mid

contains :: Box -> Point Double -> Bool
contains (Box (Point minX minY) (Point maxX maxY)) (Point x y) =
  x > minX && x < maxX && y > minY && y < maxY

{- Quadtree definition & constructor -}
data Quadtree a = Node [Quadtree a] | Leaf (Box, [a])

splitRoot :: Int -> Box -> [Point Double] -> Quadtree (Point Double)
splitRoot maxLeafSize box elts =
  split (box, elts)
  where
    partition (lu, ru, rd, ld) es =
      [ (lu, belongs lu es),
        (ru, belongs ru es),
        (rd, belongs rd es),
        (ld, belongs ld es)
      ]
    belongs box = filter (contains box)
    split (box, es) =
      if length es >= maxLeafSize
        then Node (split <$> partition (quarter box) es)
        else Leaf (box, es)

build :: (Int, Int) -> Int -> [Point Double] -> Quadtree (Point Double)
build (width, height) maxLeafSize = splitRoot maxLeafSize root
  where
    root = Box (splat 0.0) (Point (fromIntegral width) (fromIntegral height))
