{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Figure where

import Control.Lens

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
        Z -> undefined
    _figureOrigin = Point { _x=0, _y=0 }
    _orientation = ODown
    cubesI = [ Cube (Point 0 0)
             , Cube (Point 0 1)
             , Cube (Point 0 2)
             , Cube (Point 0 3)]

moveFigure :: Vector -> Figure -> Figure
moveFigure offset = figureOrigin `over` movePoint offset

updateFigure :: Figure -> Figure
updateFigure = undefined
