{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Package 
    ( Package (
          Package
        , killID
        , killmail
        , zkb
        )
    ) where

import GHC.Generics
import Data.Aeson

import Data.Zkill.Killmail
import Data.Zkill.Zkillboard

data Package = Package {
      killID :: Int
    , killmail :: Killmail
    , zkb :: Zkillboard
    } deriving (Generic, Show, Eq)

instance ToJSON Package where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Package where
