{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.ApiObject 
    ( ApiObject (
          ApiObject
        , package
        )
    ) where

import GHC.Generics
import Data.Aeson

import Data.Zkill.Package

data ApiObject = ApiObject {
    package :: Maybe Package
    } deriving (Generic, Show, Eq)

instance ToJSON ApiObject where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ApiObject where
