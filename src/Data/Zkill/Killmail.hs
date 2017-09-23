{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Killmail 
    ( Killmail (
          Killmail
        , solarSystem
        , killID
        , killTime
        , attackers
        , attackerCount
        , victim
        , killID_str
        , attackerCount_str
        , war
        )
    ) where

import GHC.Generics
import Data.Aeson
import Data.Text

import Data.Zkill.Attacker
import Data.Zkill.SolarSystem
import Data.Zkill.Victim
import Data.Zkill.War

data Killmail = Killmail {
      solarSystem :: SolarSystem
    , killID :: Int
    , killTime :: Text
    , attackers :: [Attacker]
    , attackerCount :: Int
    , victim :: Victim
    , killID_str :: Text
    , attackerCount_str :: Text
    , war :: War
    } deriving (Generic, Show, Eq)

instance ToJSON Killmail where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Killmail where
