{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Zkill.Icon 
    ( Icon (
          Icon
        , href
        )
    ) where

import GHC.Generics
import Data.Aeson
import Data.Text

data Icon = Icon {
    href :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON Icon where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Icon where
