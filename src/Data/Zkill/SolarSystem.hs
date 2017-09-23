{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.SolarSystem 
    ( SolarSystem (
          SolarSystem
        , id_str
        , href
        , id
        , name
        )
    ) where

import GHC.Generics
import Data.Aeson
import Data.Text

data SolarSystem = SolarSystem {
      id_str :: Text
    , href :: Text
    , id :: Int
    , name :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON SolarSystem where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON SolarSystem where
