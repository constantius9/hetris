{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Figure where

import Control.Lens
import Control.Monad

import Coordinate
import Cube
import Point
import Orientation
import Vector

data FigureKind = I
                | J
                | L
                | O
                | S
                | T
                | Z

data Figure = Figure
              { _cubes :: [Cube]
              , _figureOrigin :: Point
              , _orientation :: Orientation }
            deriving (Show)
$(makeLenses ''Figure)

getOffsetCubes f =
  offsetCubes'
  where
    cubes' = view cubes f :: [Cube]
    points' = map (view cubeOrigin) cubes' :: [Point]
    x' = getX (view figureOrigin f) :: Coordinate
    y' = getY (view figureOrigin f) :: Coordinate
    offset' = Vector x' y' :: Vector
    offsetByOrigin = movePoint offset' :: Point -> Point
    offsetPoints' = map offsetByOrigin points' :: [Point]
    offsetCubes' = map Cube offsetPoints' :: [Cube]

offsetCubes :: Functor f => ([Cube] -> f [Cube]) -> Figure -> f Figure
offsetCubes = undefined

createFigure kind =
  Figure { _cubes
         , _figureOrigin
         , _orientation }
  where
    _cubes =
      case kind of
        I -> cubesI
        J -> undefined
        L -> undefined
        O -> undefined
        S -> undefined
        T -> undefined
        Z -> cubesZ
    _figureOrigin = Point { _x=0, _y=0 }
    _orientation = ODown
    cubesI = [ Cube (Point 0 0)
             , Cube (Point 0 1)
             , Cube (Point 0 2)
             , Cube (Point 0 3) ]
    cubesZ = [ Cube (Point 0 0)
             , Cube (Point 1 0)
             , Cube (Point 1 1)
             , Cube (Point 1 2) ]

moveFigure :: Vector -> Figure -> Figure
moveFigure offset = figureOrigin `over` movePoint offset

data Stop = CollidedWithFallen
          | CollidedWithBorder

data Borders = Borders
               { _topLeft :: Point
               , _lowerRight  :: Point }
             deriving (Show)

data Collision = Collision Point
               deriving (Show)

makeBBox :: Figure -> Borders
makeBBox figure = Borders topLeft lowerRight
  where topLeft    = Point minX minY
        lowerRight = Point maxX maxY
        cubes' = view cubes figure :: [Cube]
        points' = map (view cubeOrigin) cubes' :: [Point]
        Point minX minY = minimum points'
        Point maxX maxY = maximum points'

propagateJust :: Maybe a -> Maybe a -> Maybe a
propagateJust (Just a) _ = (Just a)
propagateJust Nothing (Just b) = Just b
propagateJust Nothing _ = Nothing

intersect :: Figure -> Figure -> Maybe Collision
intersect f1 f2 =
  collision
  where pairs = [(b1, b2) | b1 <- getOffsetCubes f1, b2 <- getOffsetCubes f2]
        compareFunction (c1, c2) = if c1 == c2 then Just c1 else Nothing
        collisionCubes = map compareFunction pairs :: [Maybe Cube]
        collisionCube = foldl propagateJust Nothing collisionCubes
        makeCollision z = Collision (view cubeOrigin z)
        collision = liftM makeCollision collisionCube

updateFigure :: Figure -> Either Stop Figure
updateFigure = undefined
