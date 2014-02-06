{-# LANGUAGE OverloadedStrings #-}

module FrameW
       ( FrameW
       , newFrameW )
       where

import Graphics.Vty.Widgets.All
import Graphics.Vty.DisplayRegion
import Graphics.Vty.Image

import Data.List
import qualified Data.Text as T

import Frame

data FrameW = FrameW
            deriving (Show)

renderFrameInternal :: DisplayRegion -> T.Text
renderFrameInternal (DisplayRegion w h) = ss
  where s  = T.concat $ replicate (wi `div` 2) " ."
        ss = T.concat $ intersperse "\n" $ replicate hi s
        wi = fromIntegral w
        hi = fromIntegral h

renderFrame _ region ctx =
  return $
    vert_cat $
      map
      (string (getNormalAttr ctx))
      (lines $ T.unpack $ renderFrameInternal region)

newFrameW :: Frame -> IO (Widget Frame)
newFrameW b = do
  wRef <- newWidget b $ \w ->
    w { render_ = renderFrame }
  return wRef
