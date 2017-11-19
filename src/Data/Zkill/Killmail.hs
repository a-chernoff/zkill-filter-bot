{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Killmail 
    ( Killmail (
          Killmail
        , solar_system_id
        , killmail_id
        , killmail_time
        , attackers
        , victim
        )
    ) where

import GHC.Generics
import Data.Aeson
import Data.Text

import Data.Zkill.Attacker
import Data.Zkill.Victim

data Killmail = Killmail {
      solar_system_id :: Int
    , killmail_id :: Int
    , killmail_time :: Text
    , attackers :: [Attacker]
    , victim :: Victim
    } deriving (Generic, Show, Eq)

instance ToJSON Killmail where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Killmail where
