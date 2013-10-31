{-# LANGUAGE TemplateHaskell #-}

module Cube where

import Control.Lens

import Point

data Cube = Cube
            { _cubeOrigin :: Point }
          deriving (Show)
$(makeLenses ''Cube)

instance Eq Cube where
  (==) c1 c2 = (view cubeOrigin c1) == (view cubeOrigin c2)
