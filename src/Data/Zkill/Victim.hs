{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Victim where

import GHC.Generics
import Data.Aeson

import Data.Zkill.Item
import Data.Zkill.Position

data Victim = Victim {
      damage_taken :: Int
    , ship_type_id :: Int
    , character_id :: Maybe Int
    , corporation_id :: Maybe Int
    , alliance_id :: Maybe Int
    , items :: [Item]
    , position :: Position
    } deriving (Generic, Show, Eq)

instance ToJSON Victim where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Victim where
