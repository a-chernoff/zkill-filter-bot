{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Character 
    ( Character (
          Character
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

data Character = Character {
      id_str :: Text
    , href :: Text
    , id :: Int
    , name :: Text
    , icon :: Icon
    } deriving (Generic, Show, Eq)

instance ToJSON Character where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Character where
