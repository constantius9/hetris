module World where

import Control.Monad.State.Lazy

import Figure

data World = World [Figure]
           deriving (Show)

createWorld = World []

updateWorld :: State World Bool
updateWorld (World list) = map updateFigure list
