{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FieldW where

import qualified Data.Map as M
import qualified Data.Text as T

import Graphics.Vty.Image
import Graphics.Vty.Widgets.All

data Field = Field (M.Map (Int,Int) FormattedText)
             deriving (Show)

newFieldW :: IO (Widget Field, Widget FocusGroup)
newFieldW = do
  tblField <- newTable
              (replicate 12 (column $ ColFixed 2))
              BorderNone
  setDefaultCellPadding tblField padNone

  borderL <- plainText (T.pack "<!") >>= withNormalAttribute (fgColor blue)
  borderR <- plainText (T.pack "!>") >>= withNormalAttribute (fgColor blue)
  borderB <- plainText (T.pack "==") >>= withNormalAttribute (fgColor blue)
  cell    <- plainText (T.pack " .") >>= withNormalAttribute (fgColor green)
  let rowField = [mkRow borderL]
              ++ replicate 10 (mkRow cell)
              ++ [mkRow borderR]
      rowBorder = [mkRow borderL]
               ++ replicate 10 (mkRow borderB)
               ++ [mkRow borderR]
      rows = replicate 20 rowField

  mapM_ (addRow tblField) rows
  addRow tblField rowBorder

  fg <- newFocusGroup
  addToFocusGroup fg tblField

  wref <- newWidget (Field M.empty) $ \w ->
    w { render_ = \_ size ctx -> render tblField size ctx}
  return (wref, fg)
