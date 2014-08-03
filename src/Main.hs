{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

import Control.Monad.State

import qualified Data.Map as M

import Debug.Trace

import Graphics.Vty.Image
import Graphics.Vty.LLInput
import Graphics.Vty.Widgets.All

import System.Exit
import System.IO

import Background
import BackgroundW
import FieldW
import Figure
import Point
import Vector

main :: IO ()
main = do
  tbl1 <- newTable [column ColAuto] BorderNone
  t1 <- plainText $ T.pack "Your level: 0"
  t2 <- plainText $ T.pack "Full lines: 0"
  t3 <- return emptyCell
  t4 <- plainText $ T.pack "SCORE: 0"
  addRow tbl1 t1
  addRow tbl1 t2
  addRow tbl1 t3
  addRow tbl1 t4

  tblHelp <- newTable [column ColAuto] BorderNone
  t1 <- plainText $ T.pack "HELP"
  t2 <- return emptyCell
  t3 <- plainText $ T.pack "A:Left"
  t4 <- plainText $ T.pack "D:Right"
  t5 <- plainText $ T.pack "Q:Rotate CCW"
  t6 <- plainText $ T.pack "E:Rotate CW"
  t7 <- plainText $ T.pack "W:Show next"
  t8 <- plainText $ T.pack "S:Speed up"
  t9 <- plainText $ T.pack "SPACE:Drop"
  addRow tblHelp t1
  addRow tblHelp t2
  addRow tblHelp t3
  addRow tblHelp t4
  addRow tblHelp t5
  addRow tblHelp t6
  addRow tblHelp t7
  addRow tblHelp t8
  addRow tblHelp t9

  tblNext <- newTable [column ColAuto] BorderNone
  t1 <- plainText $ T.pack "Next:"
  addRow tblNext t1

  (field, _) <- newFieldW
  field `onKeyPressed` f
  vBoxCentral <- return field <--> plainText (T.pack "Play Tetris!")

  hBox1 <- return vBoxCentral <++> plainText (T.pack "Right 2")
  vBoxInnerLeft <- return tbl1 <--> return tblHelp
  vBoxLeft <- return vBoxInnerLeft <--> return tblNext
  hBox2 <- return vBoxLeft <++> return hBox1
  ui <- centered hBox2

  fg <- newFocusGroup
  fg `onKeyPressed` handleKeyPress

  addToFocusGroup fg hBox2

  c <- newCollection
  _ <- addToCollection c ui fg

  runUi c defaultContext

handleKeyPress :: Widget a -> Key -> [Modifier] -> IO Bool
handleKeyPress _ key _ =
  case key of
    KASCII 'q' -> exitSuccess
    _ -> return False

f :: Widget Field -> Key -> [Modifier] -> IO Bool
f this key _ =
  case key of
    KASCII ' ' -> do
      spawnFigureOnField this
      return True
    KASCII 'r' -> do
      removeFigureFromField this
      renderOnField this defaultText (Point 0 0)
      return True
    KLeft -> do
      f <- getActiveFigure this
      b <- getBorder this VL
      let c = intersect f b
      case (trace ("c " ++ show c) c) of
        Nothing -> do
          let Right f' = updateFigure (Move (Vector 0 (-1))) f
          replaceActiveFigure this f'
          return True
        _ -> return True
    KRight -> do
      f <- getActiveFigure this
      b <- getBorder this VR
      let c = intersect f b
      case (trace ("c " ++ show c) c) of
        Nothing -> do
          let Right f' = updateFigure (Move (Vector 0 1)) f
          replaceActiveFigure this f'
          return True
        _ -> return True
    KDown -> do
      f <- getActiveFigure this
      b <- getBorder this HB
      let c = intersect f b
      case (trace ("c " ++ show c) c) of
        Nothing -> do
          let Right f' = updateFigure (Move (Vector 1 0)) f
          replaceActiveFigure this f'
          return True
        _ -> return True

    _ -> return False
