{-# LANGUAGE OverloadedStrings #-}

module BackgroundW
       ( BackgroundW
       , newBackgroundW )
       where

import Graphics.Vty.Widgets.All
import Graphics.Vty.DisplayRegion
import Graphics.Vty.Image

import Data.List
import qualified Data.Text as T

import Background

data BackgroundW = BackgroundW
                 deriving (Show)

renderBackgroundInternal :: DisplayRegion -> T.Text
renderBackgroundInternal (DisplayRegion w h) = ss
  where s  = T.concat $ replicate (wi `div` 2) " ."
        ss = T.concat $ intersperse "\n" $ replicate hi s
        wi = fromIntegral w
        hi = fromIntegral h

renderBackground _ region ctx =
  return $
    vert_cat $
      map
      (string (getNormalAttr ctx))
      (lines $ T.unpack $ renderBackgroundInternal region)

newBackgroundW :: Background -> IO (Widget Background)
newBackgroundW b = do
  wRef <- newWidget b $ \w ->
    w { render_ = renderBackground }
  return wRef
