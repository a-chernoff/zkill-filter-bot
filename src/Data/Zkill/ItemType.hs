{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.ItemType
    ( ItemType (
          ItemType
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

data ItemType = ItemType {
      id_str :: Text
    , href :: Text
    , id :: Int
    , name :: Text
    , icon :: Icon
    } deriving (Generic, Show, Eq)

instance ToJSON ItemType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ItemType where
