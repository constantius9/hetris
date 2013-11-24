import qualified Data.Text as T

import Graphics.Vty.Widgets.All
import Graphics.Vty.LLInput

import System.Exit

main :: IO ()
main = do
  tbl1 <- newTable [column ColAuto] BorderNone
  t1 <- plainText $ T.pack "Your level: 0"
  addRow tbl1 t1
  hBox1 <- plainText (T.pack "Left 1") <++> plainText (T.pack "Right 2")
  hBox2 <- return tbl1 <++> return hBox1
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
