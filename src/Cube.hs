{-# LANGUAGE TemplateHaskell #-}

module Cube where

import Control.Lens

import Point

data Cube = Cube
            { _cubeOrigin :: Point }
          deriving (Show)
$(makeLenses ''Cube)
