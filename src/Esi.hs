{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Esi where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Simple

esiRequest :: Request
esiRequest = setRequestMethod "GET"
    . setRequestSecure True
    . setRequestPort 443
    . setRequestHost "esi.tech.ccp.is"
    . addRequestHeader "content-language" "en-us"
    . addRequestHeader "accept" "application/json"
    $ defaultRequest

type TypeId = Int
type GroupId = Int
type AllianceId = Int
type CharacterId = Int
type CorporationId = Int
type RegionId = Int
type SolarSystemId = Int

data EsiSearchType =
      Alliance
    | Character
    | Corporation
    | SolarSystem
    | Region
    | InventoryType
    deriving (Show)

data EsiSearchResult = EsiSearchResult {
      alliance      :: Maybe [AllianceId]
    , character     :: Maybe [CharacterId]
    , corporation   :: Maybe [CorporationId]
    , solarsystem   :: Maybe [SolarSystemId]
    , region        :: Maybe [RegionId]
    , inventorytype :: Maybe [TypeId]
} deriving (Show, Generic, Eq)

instance ToJSON EsiSearchResult where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON EsiSearchResult where

toCategoryQuery :: EsiSearchType -> (B.ByteString, Maybe B.ByteString)
toCategoryQuery Alliance      = ("categories", Just "alliance")
toCategoryQuery Character     = ("categories", Just "character")
toCategoryQuery Corporation   = ("categories", Just "corporation")
toCategoryQuery SolarSystem   = ("categories", Just "solarsystem")
toCategoryQuery Region        = ("categories", Just "region")
toCategoryQuery InventoryType = ("categories", Just "inventorytype")

search :: T.Text -> [EsiSearchType] -> IO EsiSearchResult
search searchStr types =
    let queryParams =  [
              ("search", Just $ TE.encodeUtf8 searchStr)
            , ("datasource", Just "tranquility")
            , ("language", Just "en-us")
            , ("strict", Just "false")
            ] ++ map toCategoryQuery types 
        req = setRequestPath "/latest/search"
            . setRequestQueryString queryParams
            $ esiRequest
    in do
        response <- httpJSONEither req
        case getResponseBody response of
            Left err -> error $ show err
            Right val -> return val

data TypeInfo = TypeInfo {
      type_id :: Int
    , name :: T.Text
    , description :: T.Text
    , published :: Bool
    , group_id :: GroupId
} deriving (Show, Generic, Eq)

instance ToJSON TypeInfo where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON TypeInfo where

getTypeInfo :: TypeId -> IO TypeInfo
getTypeInfo esiId = 
    let req = setRequestPath (B.append "/latest/universe/types/" (TE.encodeUtf8 . T.pack $ show esiId))
            $ esiRequest
    in do
        response <- httpJSONEither req
        case getResponseBody response of
            Left err -> fail $ show err
            Right val -> return val


getCharacterName :: CharacterId -> IO T.Text
getCharacterName charId =
    let req = setRequestPath (B.append "/latest/characters/" (TE.encodeUtf8 . T.pack $ show charId))
            $ esiRequest
    in do
        response <- httpJSONEither req
        case getResponseBody response of
            Left err -> fail $ show err
            Right val -> case flip parseEither val $ \obj -> obj .: "name" of
                Left err -> fail $ show err
                Right charName -> return charName
