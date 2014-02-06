module FieldW where

import Graphics.Vty.Widgets.All

import BackgroundW
import FrameW
import FigureW

data FieldW = FieldW BackgroundW FrameW FigureW FigureW
