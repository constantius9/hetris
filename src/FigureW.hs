module FigureW
       ( FigureW
       , newFigureW )
       where

import qualified Data.Text as T

import Graphics.Vty.Widgets.All
import Graphics.Vty.Image

import Figure
import FieldW

data FigureW = FigureW Figure

newFigureW :: Widget Field -> Figure -> IO (Widget Figure)
newFigureW field f = do
  wRef <- newWidget f $ \w ->
    w { render_ =
           \this _ ctx -> do
             s <- getState this
             renderOnField field (draw s) (0,0)
             return () }
  return wRef
