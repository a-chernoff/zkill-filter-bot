{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Item where

import GHC.Generics
import Data.Aeson

data Item = Item {
      singleton :: Int
    , item_type_id :: Int
    , flag :: Int
    , quantity_dropped :: Maybe Int
    , quantity_destroyed :: Maybe Int
    } deriving (Generic, Show, Eq)

instance ToJSON Item where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Item where
