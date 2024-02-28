{-# LANGUAGE DeriveFunctor #-}

module Point where

import Control.Applicative (Applicative (liftA2))
import qualified System.Random as Random

{- Point type & utilities -}
data Point a = Point
  { x :: a,
    y :: a
  }
  deriving (Show, Functor)

instance Applicative Point where
  pure a = Point a a
  (Point f1 f2) <*> (Point x y) = Point (f1 x) (f2 y)

instance (Random.Random a, Num a) => Random.Random (Point a) where
  random g =
    let (x, g') = Random.random g
        (y, g'') = Random.random g'
     in (Point x y, g'')
  randomR (Point lx ly, Point hx hy) g =
    let (x, g') = Random.randomR (lx, hx) g
        (y, g'') = Random.randomR (ly, hy) g'
     in (Point x y, g'')

splat :: a -> Point a
splat a = Point a a

map2 :: (a -> a -> b) -> Point a -> Point a -> Point b
map2 f (Point x1 y1) (Point x2 y2) = Point (f x1 x2) (f y1 y2)

toPoint :: (Num a) => (a, a) -> Point a
toPoint (x, y) =
  Point x y

(+~) :: (Num a) => Point a -> Point a -> Point a
(+~) = liftA2 (+)

(/!) :: Point Double -> Double -> Point Double
(Point x y) /! scalar = Point (x / scalar) (y / scalar)
