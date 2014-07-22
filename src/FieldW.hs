{-# LANGUAGE OverloadedStrings #-}

module FieldW where

import qualified Data.Map as M
import qualified Data.Text as T

import Graphics.Vty.Image
import Graphics.Vty.Widgets.All

data Character = Character { glyph :: T.Text
                           , color :: Color
                           } deriving (Show)

type FieldMap = M.Map (Int,Int) Character

data Field = Field (M.Map (Int,Int) Character) (Widget Table)

instance Show Field where
  show (Field m _) = show m

newFieldW :: IO (Widget Field, Widget FocusGroup)
newFieldW = do
  tblField <- newTable
              (replicate 12 (column $ ColFixed 2))
              BorderNone
  setDefaultCellPadding tblField padNone

  let cells   = [(i, Character " ." green) | i <- [1..10]]
      row     = [(0, Character "<!" blue)] ++ cells ++ [(11, Character "!>" blue)]
      borderB = [(0,  Character "<!" blue)]
             ++ [(i,  Character "==" blue) | i <- [1..10]]
             ++ [(11, Character "!>" blue)]
      rowsCells = [((i,j), c) | i <- [0..19] :: [Int], (j, c) <- row]
      rowBorder = [((20,j), c) | (j, c) <- borderB]
      field = M.fromList $ rowsCells ++ rowBorder :: FieldMap
      rows = [M.toAscList $ M.filterWithKey (\k _ -> (== i) . fst $ k) field | i <- [0..20]] :: [[((Int,Int), Character)]]
      rows' = [map snd l | l <- rows] :: [[Character]]

  mapM_ ((\cs -> do ws <- mapM (\(Character g c) -> plainText g >>= withNormalAttribute (fgColor c)) cs
                    let r = foldl (.|.) (mkRow . head $ ws) (tail ws)
                    addRow tblField r) :: [Character] -> IO ()) rows'

  fg <- newFocusGroup
  addToFocusGroup fg tblField

  wref <- newWidget (Field field tblField) $ \w ->
    w { render_ = \this size ctx -> do
           (Field m w) <- getState this
           render tblField size ctx}
  return (wref, fg)
