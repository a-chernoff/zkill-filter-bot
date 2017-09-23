{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Position 
    ( Position (
          Position
        , x
        , y
        , z
        )
    ) where

import GHC.Generics
import Data.Aeson
import Data.Text

data Position = Position {
      y :: Double
    , x :: Double
    , z :: Double
    } deriving (Generic, Show, Eq)

instance ToJSON Position where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Position where
