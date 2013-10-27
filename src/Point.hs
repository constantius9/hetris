{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Point where

import Control.Lens
import Data.AffineSpace

import Coordinate
import Vector

data Point = Point
             { _x :: Coordinate
             , _y :: Coordinate }
           deriving (Show)

$(makeLenses ''Point)

movePoint :: Vector -> Point -> Point
movePoint offset = (.+^ offset)

instance AffineSpace Point where
  type Diff Point = Vector
  (Point x1 y1) .-. (Point x2 y2) = Vector (x2 - x1) (y2 - y1)
  (Point x1 y1) .+^ (Vector x' y') = Point (x1 + x') (y1 + y')
