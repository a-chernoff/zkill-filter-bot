{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Corporation 
    ( Corporation (
          Corporation
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

data Corporation = Corporation {
      id_str :: Text
    , href :: Text
    , id :: Int
    , name :: Text
    , icon :: Icon
    } deriving (Generic, Show, Eq)

instance ToJSON Corporation where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Corporation where
