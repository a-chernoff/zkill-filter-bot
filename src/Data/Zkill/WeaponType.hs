{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.WeaponType 
    ( WeaponType (
          WeaponType
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

data WeaponType = WeaponType {
      id_str :: Text
    , href :: Text
    , id :: Int
    , name :: Text
    , icon :: Icon
    } deriving (Generic, Show, Eq)

instance ToJSON WeaponType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON WeaponType where
