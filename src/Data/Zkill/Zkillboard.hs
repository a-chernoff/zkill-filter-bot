{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Zkillboard 
    ( Zkillboard (
          Zkillboard
        , locationID
        , hash
        , fittedValue
        , totalValue
        , points
        , npc
        , href
        )
    ) where

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
    , href :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON Zkillboard where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Zkillboard where
