{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Array

newtype Score = Score Int
              deriving (Num, Show)

newtype Speed = Speed Int
              deriving (Num, Show)

newtype Framerate = Framerate Int
                  deriving (Num, Show)

newtype Index = Index Int
              deriving (Enum, Eq, Ix, Num, Ord, Show)
