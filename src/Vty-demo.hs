import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import FigureW
import Figure
import BackgroundW
import Background

main :: IO ()
main = do
  e <- newBackgroundW (Background)
  ui <- boxFixed 20 10 e
--  ui <- centered e

  fg <- newFocusGroup
  addToFocusGroup fg e

  c <- newCollection
  addToCollection c ui fg

--  e `onActivate` \this ->
--    getEditText this >>= (error . ("You entered: " ++) . T.unpack)

  runUi c defaultContext
