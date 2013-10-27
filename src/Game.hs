{-# LANGUAGE TemplateHaskell #-}

module Game where

import Types
import World

data Game = Game
            { _score :: Score
            , _speed :: Speed
            , _framerate :: Framerate
            , _world :: World }

$(makeLenses ''Game)

createGame = Game { _score=0
                  , _speed=30
                  , _framerate=30
                  , _world=createWorld}
