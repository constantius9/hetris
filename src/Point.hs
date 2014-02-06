{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Point where

import Control.Lens

import Data.AffineSpace
import Data.Function
import Data.List

import Coordinate
import Vector

data Point = Point
             { _x :: Coordinate
             , _y :: Coordinate }
           deriving (Show)

$(makeLenses ''Point)

getX = view x
getY = view y

instance Eq Point where
  (==) a b = (getX a == getX b) && (getY a == getY b)

instance Ord Point where
  compare a b = compare (getY a, getX a) (getY b, getX b)

instance AffineSpace Point where
  type Diff Point = Vector
  (Point x1 y1) .-. (Point x2 y2) = Vector (x2 - x1) (y2 - y1)
  (Point x1 y1) .+^ (Vector x' y') = Point (x1 + x') (y1 + y')

movePoint :: Vector -> Point -> Point
movePoint offset = (.+^ offset)

movePointR = flip movePoint

foldWith accessor processor list =
  accessor $ processor (compare `on` accessor) list

minimumByX = foldWith getX minimumBy
minimumByY = foldWith getY minimumBy
maximumByX = foldWith getX maximumBy
maximumByY = foldWith getY maximumBy

pointsBetween :: Point -> Point -> [Point]
pointsBetween a b = map (uncurry Point) [(c, d) | c <- xs, d <- ys]
  where minX = minimumByX [a, b]
        minY = minimumByY [a, b]
        maxX = maximumByX [a, b]
        maxY = maximumByY [a, b]
        xs   = [minX .. maxX]
        ys   = [minY .. maxY]
