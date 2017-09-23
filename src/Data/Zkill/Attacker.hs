{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Attacker 
    ( Attacker (
          Attacker
        , alliance
        , shipType
        , corporation
        , character
        , damageDone_str
        , weaponType
        , finalBlow
        , securityStatus
        , damageDone
        )
    ) where

import GHC.Generics
import Data.Aeson
import Data.Text

import Data.Zkill.Alliance
import Data.Zkill.Character
import Data.Zkill.ShipType
import Data.Zkill.Corporation
import Data.Zkill.WeaponType

data Attacker = Attacker {
      alliance :: Maybe Alliance
    , shipType :: Maybe ShipType
    , corporation :: Maybe Corporation
    , character :: Maybe Character
    , damageDone_str :: Text
    , weaponType :: Maybe WeaponType
    , finalBlow :: Bool
    , securityStatus :: Float
    , damageDone :: Int
    } deriving (Generic, Show, Eq)

instance ToJSON Attacker where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Attacker where
