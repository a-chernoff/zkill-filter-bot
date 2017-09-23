{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Alliance 
    ( Alliance (Alliance, id_str, href, id, name, icon)
    ) where

import GHC.Generics
import Data.Aeson
import Data.Text

import Data.Zkill.Icon

data Alliance = Alliance {
      id_str :: Text
    , href :: Text
    , id :: Int
    , name :: Text
    , icon :: Icon
    } deriving (Generic, Show, Eq)

instance ToJSON Alliance where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Alliance where
