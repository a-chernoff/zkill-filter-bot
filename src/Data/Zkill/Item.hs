{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Item
    ( Item (
          Item
        , singleton
        , itemType
        , quantityDropped_str
        , flag
        , singleton_str
        , quantityDropped
        , flag_str
        )
    ) where

import GHC.Generics
import Data.Aeson
import Data.Text

import Data.Zkill.ItemType

data Item = Item {
      singleton :: Int
    , itemType :: ItemType
    , quantityDropped_str :: Maybe Text
    , flag :: Int
    , singleton_str :: Text
    , quantityDropped :: Maybe Int
    , flag_str :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON Item where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Item where
