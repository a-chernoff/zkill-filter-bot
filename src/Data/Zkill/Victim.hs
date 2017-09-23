{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Victim 
    ( Victim (
          Victim
        , alliance
        , damageTaken
        , items
        , damageTaken_str
        , character
        , shipType
        , corporation
        , position
        )
    ) where

import GHC.Generics
import Data.Aeson
import Data.Text

import Data.Zkill.Alliance
import Data.Zkill.Character
import Data.Zkill.Item
import Data.Zkill.ShipType
import Data.Zkill.Corporation
import Data.Zkill.Position

data Victim = Victim {
      alliance :: Maybe Alliance
    , damageTaken :: Int
    , items :: [Item]
    , damageTaken_str :: Text
    , character :: Maybe Character
    , shipType :: ShipType
    , corporation :: Corporation
    , position :: Position
    } deriving (Generic, Show, Eq)

instance ToJSON Victim where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Victim where
