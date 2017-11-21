{- LANGUAGE GeneralizedNewtypeDeriving #-}
module Filter where

import Data.Maybe (fromMaybe, maybe)

import Data.Zkill.Killmail
import Data.Zkill.Package
import Data.Zkill.Victim as V
import Data.Zkill.Attacker as A
import Data.Zkill.Zkillboard

newtype Filter = Filter {
    runFilter :: Package -> Bool
}

newtype Attackers = Attackers {
    getAttackers :: [Attacker]
}

class Character a where
    matchCharacterId :: Int -> a -> Bool
    matchCorporationId :: Int -> a -> Bool
    matchAllianceId :: Int -> a -> Bool
    matchShipTypeId :: Int -> a -> Bool

instance Character Victim where
    matchCharacterId i v = maybe False (== i) (V.character_id v)
    matchCorporationId i v = maybe False (== i) (V.corporation_id v)
    matchAllianceId i v = maybe False (== i) (V.alliance_id v)
    matchShipTypeId i v = i == V.ship_type_id v

instance Character Attackers where
    matchCharacterId cId = any (\atk -> maybe False (== cId) (A.character_id atk)) . getAttackers
    matchCorporationId cId = any (\atk -> maybe False (== cId) (A.corporation_id atk)) . getAttackers
    matchAllianceId aId = any (\atk -> maybe False (== aId) (A.alliance_id atk)) . getAttackers
    matchShipTypeId sId = any (\atk -> maybe False (== sId) (A.ship_type_id atk)) . getAttackers

characterFilter :: Character a => Int -> (Package -> a) -> Filter
characterFilter cId f = Filter $ \api -> matchCharacterId cId (f api)

corporationFilter :: Character a => Int -> (Package -> a) -> Filter
corporationFilter cId f = Filter $ \api -> matchCorporationId cId (f api)

allianceFilter :: Character a => Int -> (Package -> a) -> Filter
allianceFilter aId f = Filter $ \api -> matchAllianceId aId (f api)

shipTypeFilter :: Character a => Int -> (Package -> a) -> Filter
shipTypeFilter sId f = Filter $ \api -> matchShipTypeId sId (f api)

solarSystemFilter :: Int -> Filter
solarSystemFilter ssId = Filter $ \api -> ssId == (solar_system_id . killmail $ api)

isNpcFilter :: Filter
isNpcFilter = Filter (npc . zkb)

-- need esi api for these two
constellationFilter :: Int -> Filter
constellationFilter = undefined

regionFilter :: Int -> Filter
regionFilter = undefined

attackerFilter :: ((Package -> Attackers) -> Filter) -> Filter
attackerFilter f = f (\a -> Attackers . attackers . killmail $ a)

victimFilter :: ((Package -> Victim) -> Filter) -> Filter
victimFilter f = f (\a -> victim . killmail $ a)

maybeFilter :: (Package -> Maybe Bool) -> Filter
maybeFilter f = Filter $ \api -> fromMaybe False (f api)

notFilter :: Filter -> Filter
notFilter f = Filter $ \api -> not (runFilter f $ api)

(.&) :: Filter -> Filter -> Filter
a .& b = Filter $ (&&) <$> runFilter a <*> runFilter b

(.|) :: Filter -> Filter -> Filter
a .| b = Filter $ (||) <$> runFilter a <*> runFilter b

infixr 9 .?
(.?) :: (b -> c) -> (a -> Maybe b) -> a -> Maybe c
(.?) f g a = do
    b <- g a
    return $ f b