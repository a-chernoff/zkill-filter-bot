{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Attacker where

import GHC.Generics
import Data.Aeson

data Attacker = Attacker {
      security_status :: Float
    , final_blow :: Bool
    , damage_done :: Int
    , character_id :: Maybe Int
    , corporation_id :: Maybe Int
    , alliance_id :: Maybe Int
    , weapon_type_id :: Maybe Int
    , ship_type_id :: Maybe Int
    } deriving (Generic, Show, Eq)

instance ToJSON Attacker where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Attacker where
