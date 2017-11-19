module Filter where

import Data.Maybe (fromMaybe)

import Data.Zkill.ApiObject
import Data.Zkill.Killmail
import Data.Zkill.Package
import Data.Zkill.Victim

type Filter = ApiObject -> Bool

solarSystemFilter :: Int -> Filter
solarSystemFilter ssId = \api -> ssId == (solar_system_id . killmail . package $ api)

shipTypeFilter :: Int -> Filter
shipTypeFilter stId = \api -> stId == (ship_type_id . victim . killmail . package $ api)

victimFilter :: Int -> Filter
victimFilter cId = maybeFilter $ \api -> (==) <$> pure cId <*> (character_id . victim . killmail . package $ api)

victimCorpFilter :: Int -> Filter
victimCorpFilter cId = maybeFilter $ \api -> (==) <$> pure cId <*> (corporation_id . victim . killmail . package $ api)

victimAllianceFilter :: Int -> Filter
victimAllianceFilter aId = maybeFilter $ \api -> (==) <$> pure aId <*> (alliance_id . victim . killmail . package $ api)

maybeFilter :: (ApiObject -> Maybe Bool) -> Filter
maybeFilter f = \api -> fromMaybe False (f api)

(.&) :: Filter -> Filter -> Filter
a .& b = (&&) <$> a <*> b

(.|) :: Filter -> Filter -> Filter
a .| b = (||) <$> a <*> b
