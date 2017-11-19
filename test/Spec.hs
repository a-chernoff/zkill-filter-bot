{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

-- import Data.Text
import Data.Aeson
-- import Data.ByteString.Lazy as BS

import qualified Data.Zkill.Alliance as Alliance
import qualified Data.Zkill.Attacker as Attacker
import qualified Data.Zkill.Character as Character
import qualified Data.Zkill.Corporation as Corporation
import qualified Data.Zkill.Icon as Icon
import qualified Data.Zkill.Item as Item
import qualified Data.Zkill.ItemType as ItemType
import qualified Data.Zkill.Killmail as Killmail
-- import qualified Data.Zkill.Package as Package
import qualified Data.Zkill.Position as Position
import qualified Data.Zkill.ShipType as ShipType
import qualified Data.Zkill.SolarSystem as SolarSystem
import qualified Data.Zkill.Victim as Victim
import qualified Data.Zkill.War as War
import qualified Data.Zkill.WeaponType as WeaponType
-- import qualified Data.Zkill.Zkillboard as Zkillboard

main :: IO ()
main = runTestTT tests >> return ()

tests :: Test
tests = TestList [
      TestLabel "Alliance Parser" testAllianceParser
    , TestLabel "Killmail Parser" testKillmailParser
    ]

testAllianceParser :: Test
testAllianceParser = TestCase (assertEqual "for eitherDecode {alliance}, "
    (Right Alliance.Alliance {
          Alliance.id_str = "99005338"
        , Alliance.href = "https://crest-tq.eveonline.com/alliances/99005338/"
        , Alliance.id = 99005338
        , Alliance.name = "Pandemic Horde"
        , Alliance.icon = Icon.Icon {
            Icon.href = "http://imageserver.eveonline.com/Alliance/99005338_128.png"
            }
        })
    (eitherDecode "{\"id_str\":\"99005338\",\"href\":\"https://crest-tq.eveonline.com/alliances/99005338/\",\"id\":99005338,\"name\":\"Pandemic Horde\",\"icon\":{\"href\":\"http://imageserver.eveonline.com/Alliance/99005338_128.png\"}}"))

testKillmailParser :: Test
testKillmailParser = TestCase (assertEqual "for eitherDecode {killmail}, "    
    (Right Killmail.Killmail {
          Killmail.solarSystem = SolarSystem.SolarSystem {
              SolarSystem.id_str = "30003688"
            , SolarSystem.href = "https://crest-tq.eveonline.com/solarsystems/30003688/"
            , SolarSystem.id = 30003688
            , SolarSystem.name = "X36Y-G"
            }
        , Killmail.killID = 64834197
        , Killmail.killTime = "2017.09.22 21:48:32"
        , Killmail.attackers = [
            Attacker.Attacker {
                  Attacker.alliance = Just Alliance.Alliance {
                      Alliance.id_str = "99005338"
                    , Alliance.href = "https://crest-tq.eveonline.com/alliances/99005338/"
                    , Alliance.id = 99005338
                    , Alliance.name = "Pandemic Horde"
                    , Alliance.icon = Icon.Icon {
                        Icon.href = "http://imageserver.eveonline.com/Alliance/99005338_128.png"
                        }
                    }
                , Attacker.shipType = Just ShipType.ShipType {
                      ShipType.id_str = "621"
                    , ShipType.href = "https://crest-tq.eveonline.com/inventory/types/621/"
                    , ShipType.id = 621
                    , ShipType.name = "Caracal"
                    , ShipType.icon = Icon.Icon {
                        Icon.href = "http://imageserver.eveonline.com/Type/621_128.png"
                        }
                    }
                , Attacker.corporation = Just Corporation.Corporation {
                      Corporation.id_str = "98388312"
                    , Corporation.href = "https://crest-tq.eveonline.com/corporations/98388312/"
                    , Corporation.id = 98388312
                    , Corporation.name = "Pandemic Horde Inc."
                    , Corporation.icon = Icon.Icon {
                        Icon.href = "http://imageserver.eveonline.com/Corporation/98388312_128.png"
                        }
                    }
                , Attacker.character = Just Character.Character {
                      Character.id_str = "94834545"
                    , Character.href = "https://crest-tq.eveonline.com/characters/94834545/"
                    , Character.id = 94834545
                    , Character.name = "Rosch Sicro"
                    , Character.icon = Icon.Icon {
                        Icon.href = "http://imageserver.eveonline.com/Character/94834545_128.jpg"
                        }
                    }
                , Attacker.damageDone_str = "2001"
                , Attacker.weaponType = Just WeaponType.WeaponType {
                      WeaponType.id_str = "27387"
                    , WeaponType.href = "https://crest-tq.eveonline.com/inventory/types/27387/"
                    , WeaponType.id = 27387
                    , WeaponType.name = "Caldari Navy Mjolnir Light Missile"
                    , WeaponType.icon = Icon.Icon {
                        Icon.href = "http://imageserver.eveonline.com/Type/27387_128.png"
                        }
                    }
                , Attacker.finalBlow = True
                , Attacker.securityStatus = 5
                , Attacker.damageDone = 2001
                }
            ]
        , Killmail.attackerCount = 1
        , Killmail.victim = Victim.Victim {
              Victim.alliance = Just Alliance.Alliance {
                  Alliance.id_str = "99002938"
                , Alliance.href = "https://crest-tq.eveonline.com/alliances/99002938/"
                , Alliance.id = 99002938
                , Alliance.name = "DARKNESS."
                , Alliance.icon = Icon.Icon {
                    Icon.href = "http://imageserver.eveonline.com/Alliance/99002938_128.png"
                    }
                }
            , Victim.damageTaken = 2001
            , Victim.items = [
                Item.Item {
                      Item.singleton = 0
                    , Item.itemType = ItemType.ItemType {
                          ItemType.id_str = "2046"
                        , ItemType.href = "https://crest-tq.eveonline.com/inventory/types/2046/"
                        , ItemType.id = 2046
                        , ItemType.name = "Damage Control I"
                        , ItemType.icon = Icon.Icon {
                            Icon.href = "http://imageserver.eveonline.com/Type/2046_128.png"
                            }
                        }
                    , Item.quantityDropped_str = Just "1"
                    , Item.flag = 11
                    , Item.singleton_str = "0"
                    , Item.quantityDropped = Just 1
                    , Item.flag_str = "11"
                    }
                ]
            , Victim.damageTaken_str = "2001"
            , Victim.character = Just Character.Character {
                  Character.id_str = "95290141"
                , Character.href = "https://crest-tq.eveonline.com/characters/95290141/"
                , Character.id = 95290141
                , Character.name = "Sergei Ellecon"
                , Character.icon = Icon.Icon {
                    Icon.href = "http://imageserver.eveonline.com/Character/95290141_128.jpg"
                    }
                }
            , Victim.shipType = ShipType.ShipType {
                  ShipType.id_str = "583"
                , ShipType.href = "https://crest-tq.eveonline.com/inventory/types/583/"
                , ShipType.id = 583
                , ShipType.name = "Condor"
                , ShipType.icon = Icon.Icon {
                    Icon.href = "http://imageserver.eveonline.com/Type/583_128.png"
                    }
                }
            , Victim.corporation = Corporation.Corporation {
                  Corporation.id_str = "98486133"
                , Corporation.href = "https://crest-tq.eveonline.com/corporations/98486133/"
                , Corporation.id = 98486133
                , Corporation.name = "SQUAD V"
                , Corporation.icon = Icon.Icon {
                    Icon.href = "http://imageserver.eveonline.com/Corporation/98486133_128.png"
                    }
                }
            , Victim.position = Position.Position {
                  Position.y = -12388625510.265
                , Position.x = -89240463348.774
                , Position.z = 180659736681.95
                }
            }
        , Killmail.killID_str = "64834197"
        , Killmail.attackerCount_str = "1"
        , Killmail.war = War.War {
              War.href = "https://crest-tq.eveonline.com/wars/0/"
            , War.id = 0
            , War.id_str = "0"
            }
        })
    (eitherDecode "{\"solarSystem\":{\"id_str\":\"30003688\",\"href\":\"https://crest-tq.eveonline.com/solarsystems/30003688/\",\"id\":30003688,\"name\":\"X36Y-G\"},\"killID\":64834197,\"killTime\":\"2017.09.22 21:48:32\",\"attackers\":[{\"alliance\":{\"id_str\":\"99005338\",\"href\":\"https://crest-tq.eveonline.com/alliances/99005338/\",\"id\":99005338,\"name\":\"Pandemic Horde\",\"icon\":{\"href\":\"http://imageserver.eveonline.com/Alliance/99005338_128.png\"}},\"shipType\":{\"id_str\":\"621\",\"href\":\"https://crest-tq.eveonline.com/inventory/types/621/\",\"id\":621,\"name\":\"Caracal\",\"icon\":{\"href\":\"http://imageserver.eveonline.com/Type/621_128.png\"}},\"corporation\":{\"id_str\":\"98388312\",\"href\":\"https://crest-tq.eveonline.com/corporations/98388312/\",\"id\":98388312,\"name\":\"Pandemic Horde Inc.\",\"icon\":{\"href\":\"http://imageserver.eveonline.com/Corporation/98388312_128.png\"}},\"character\":{\"id_str\":\"94834545\",\"href\":\"https://crest-tq.eveonline.com/characters/94834545/\",\"id\":94834545,\"name\":\"Rosch Sicro\",\"icon\":{\"href\":\"http://imageserver.eveonline.com/Character/94834545_128.jpg\"}},\"damageDone_str\":\"2001\",\"weaponType\":{\"id_str\":\"27387\",\"href\":\"https://crest-tq.eveonline.com/inventory/types/27387/\",\"id\":27387,\"name\":\"Caldari Navy Mjolnir Light Missile\",\"icon\":{\"href\":\"http://imageserver.eveonline.com/Type/27387_128.png\"}},\"finalBlow\":true,\"securityStatus\":5,\"damageDone\":2001}],\"attackerCount\":1,\"victim\":{\"alliance\":{\"id_str\":\"99002938\",\"href\":\"https://crest-tq.eveonline.com/alliances/99002938/\",\"id\":99002938,\"name\":\"DARKNESS.\",\"icon\":{\"href\":\"http://imageserver.eveonline.com/Alliance/99002938_128.png\"}},\"damageTaken\":2001,\"items\":[{\"singleton\":0,\"itemType\":{\"id_str\":\"2046\",\"href\":\"https://crest-tq.eveonline.com/inventory/types/2046/\",\"id\":2046,\"name\":\"Damage Control I\",\"icon\":{\"href\":\"http://imageserver.eveonline.com/Type/2046_128.png\"}},\"quantityDropped_str\":\"1\",\"flag\":11,\"singleton_str\":\"0\",\"quantityDropped\":1,\"flag_str\":\"11\"}],\"damageTaken_str\":\"2001\",\"character\":{\"id_str\":\"95290141\",\"href\":\"https://crest-tq.eveonline.com/characters/95290141/\",\"id\":95290141,\"name\":\"Sergei Ellecon\",\"icon\":{\"href\":\"http://imageserver.eveonline.com/Character/95290141_128.jpg\"}},\"shipType\":{\"id_str\":\"583\",\"href\":\"https://crest-tq.eveonline.com/inventory/types/583/\",\"id\":583,\"name\":\"Condor\",\"icon\":{\"href\":\"http://imageserver.eveonline.com/Type/583_128.png\"}},\"corporation\":{\"id_str\":\"98486133\",\"href\":\"https://crest-tq.eveonline.com/corporations/98486133/\",\"id\":98486133,\"name\":\"SQUAD V\",\"icon\":{\"href\":\"http://imageserver.eveonline.com/Corporation/98486133_128.png\"}},\"position\":{\"y\":-12388625510.265,\"x\":-89240463348.774,\"z\":180659736681.95}},\"killID_str\":\"64834197\",\"attackerCount_str\":\"1\",\"war\":{\"href\":\"https://crest-tq.eveonline.com/wars/0/\",\"id\":0,\"id_str\":\"0\"}}"))