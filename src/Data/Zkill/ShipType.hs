{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.ShipType 
    ( ShipType (
          ShipType
        , id_str
        , href
        , id
        , name
        , icon
        )
    ) where

import GHC.Generics
import Data.Aeson
import Data.Text

import Data.Zkill.Icon

data ShipType = ShipType {
      id_str :: Text
    , href :: Text
    , id :: Int
    , name :: Text
    , icon :: Icon
    } deriving (Generic, Show, Eq)

instance ToJSON ShipType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ShipType where
