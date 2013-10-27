{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coordinate where

import Data.AdditiveGroup

newtype Coordinate = Coordinate Int
                   deriving ( AdditiveGroup
                            , Enum
                            , Eq
                            , Integral
                            , Num
                            , Ord
                            , Real
                            , Show)
