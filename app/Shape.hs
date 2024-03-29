module Shape
  ( Shape (..),
    toFlatShapes,
  )
where

import Point (Point (..))
import Tree (Box (..), Quadtree (..), midpoint)

{- Shape type & Quadtree -> [Shape] function for visualization -}
data Shape = Circle {cc :: Point Double, radius :: Double} | Rectangle {rc :: Point Double, width :: Double, height :: Double}

toFlatShapes :: Quadtree (Point Double) -> [Shape]
toFlatShapes = convert
  where
    convert (Node children) = concatMap convert children
    convert (Leaf (bb, es)) =
      let circles = map circleOfPoint es
          rect = rectOfBB bb
       in circles ++ [rect]
    circleOfPoint p = Circle p 3
    rectOfBB bb@(Box min max) =
      let (Point cx cy) = midpoint bb
          w = x max - x min
          h = y max - y min
          center = Point (cx - w / 2.0) (cy - h / 2.0) :: Point Double
       in Rectangle center w h
