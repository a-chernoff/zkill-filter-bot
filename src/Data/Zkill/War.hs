{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.War 
    ( War (
          War
        , href
        , id
        , id_str
        )
    ) where

import GHC.Generics
import Data.Aeson
import Data.Text

data War = War {
      href :: Text
    , id :: Int
    , id_str :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON War where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON War where
