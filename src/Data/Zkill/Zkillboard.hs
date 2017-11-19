{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Zkillboard where

import GHC.Generics
import Data.Aeson
import Data.Text

data Zkillboard = Zkillboard {
      locationID :: Int
    , hash :: Text
    , fittedValue :: Float
    , totalValue :: Float
    , points :: Int
    , npc :: Bool
    , solo :: Bool
    , awox :: Bool
    , href :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON Zkillboard where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Zkillboard where
