{-# LANGUAGE TypeFamilies #-}

module Vector where

import Data.AffineSpace()
import Data.VectorSpace

import Coordinate

data Vector = Vector { _xv :: Coordinate
                     , _yv :: Coordinate
                     } deriving (Show, Eq, Ord)

instance AdditiveGroup Vector where
  zeroV = Vector 0 0
  (Vector x1 y1) ^+^ (Vector x2 y2) = Vector (x1+x2) (y1+y2)
  negateV (Vector x1 y1) = Vector (-x1) (-y1)

instance VectorSpace Vector where
  type Scalar Vector = Coordinate
  k *^ (Vector x1 y1) = Vector (k*x1) (k*y1)

instance InnerSpace Vector where
  (Vector x1 y1) <.> (Vector x2 y2) = x1*x2 + y1*y2

euclideanNorm :: Vector -> Double
euclideanNorm = sqrt . fromIntegral . magnitudeSq
