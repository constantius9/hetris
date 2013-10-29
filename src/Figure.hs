{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Figure where

import Control.Lens

import Constants
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

data Collision = Collision Point Point

intersect = undefined

makeBBox :: Figure -> Borders
makeBBox figure = Borders topLeft lowerRight
  where topLeft    = Point minX minY
        lowerRight = Point maxX maxY
        cubes' = view cubes figure :: [Cube]
        points' = map (view cubeOrigin) cubes' :: [Point]
        Point minX minY = minimum points'
        Point maxX maxY = maximum points'

collideWithBorder :: Borders -> Figure -> Maybe Collision
collideWithBorder borders figure = borders `intersect` bbox
  where bbox = makeBBox figure

updateFigure :: Figure -> Either Stop Figure
updateFigure = undefined
