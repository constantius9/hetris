{-# LANGUAGE OverloadedStrings #-}

module FieldW
       ( Field(..)
       , newFieldW
       , renderOnField )
       where

import qualified Data.Map as M
import qualified Data.Text as T

import Graphics.Vty.Image
import Graphics.Vty.Widgets.All

data Character = Character { glyph :: T.Text
                           , color :: Color
                           } deriving (Show)

type FieldMap = M.Map (Int,Int) Character

data Field = Field (M.Map (Int,Int) (Widget FormattedText)) (Widget Table)

instance Show Field where
  show (Field _ _) = "Field"

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

  wms <- mapM ((\cs -> do ws <- mapM (\(_, Character g c) -> plainText g >>= withNormalAttribute (fgColor c)) cs
                          let r = foldl (.|.) (mkRow . head $ ws) (tail ws)
                              widgetsMap = M.fromList $ zip (map fst cs) ws :: M.Map (Int, Int) (Widget FormattedText)
                          addRow tblField r
                          return widgetsMap) :: [((Int,Int), Character)] -> IO (M.Map (Int, Int) (Widget FormattedText))) rows

  let wm = foldl1 (M.union) wms

  fg <- newFocusGroup
  addToFocusGroup fg tblField

  wref <- newWidget (Field wm tblField) $ \w ->
    w { render_ = \_ size ctx -> render tblField size ctx}
  return (wref, fg)


renderOnField :: Widget Field -> T.Text -> (Int, Int) -> IO ()
renderOnField field text pos@(oi,oj) = do
  Field wm _ <- getState field
  mapM_ (\(i,t) -> setText (wm M.! i) t) ls''''
  where ls = T.lines text
        ls' = map (replicate 2) ls :: [[T.Text]]
        ls'' = map (\l -> if T.length (head l) /= 2 then [T.concat l] else l) ls' :: [[T.Text]]
        ls''' = zip [0..] $ map (zip [0..]) ls''
        ls'''' = concatMap (\(i,l) -> map (\(j,t) -> ((oi+i,oj+j),t)) l) ls''' :: [((Int,Int), T.Text)]
