import qualified Data.Text as T

import Graphics.Vty.Image
import Graphics.Vty.LLInput
import Graphics.Vty.Widgets.All

import System.Exit

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

  tblField <- newTable
              [ column $ ColFixed 48]
              BorderNone
  setDefaultCellPadding tblField padNone

  t1 <- plainText (T.pack "|| . . . . . . . . . . . . . . . . . . . .||") >>= withNormalAttribute (fgColor blue)
  addRow tblField t1

  vBoxCentral <- return tblField <--> plainText (T.pack "Play Tetris!")

  hBox1 <- return vBoxCentral <++> plainText (T.pack "Right 2")
  vBoxInnerLeft <- return tbl1 <--> return tblHelp
  vBoxLeft <- return vBoxInnerLeft <--> return tblNext
  hBox2 <- return vBoxLeft <++> return hBox1
  ui <- centered hBox2

  fg <- newFocusGroup
  fg `onKeyPressed` \_ key _ ->
    if key == KASCII 'q'
    then exitSuccess
    else return False

  addToFocusGroup fg hBox2

  c <- newCollection
  _ <- addToCollection c ui fg

  runUi c defaultContext
