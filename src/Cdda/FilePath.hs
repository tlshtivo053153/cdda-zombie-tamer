module Cdda.FilePath where

import System.FilePath
import qualified Data.Text as T

import Define.Core

getModInfo :: FilePath
getModInfo = "modinfo.json"

getItemFood :: FilePath
getItemFood = "items"</>"petfood.json"

getMonsterVanilla :: FilePath
getMonsterVanilla = "monster"</>"vanilla.json"

getMonsterFriend :: Id -> FilePath
getMonsterFriend (Id i) = "monster"</>"friend"</> T.unpack i <> ".json"

getTalkVanilla :: FilePath
getTalkVanilla = "talk"</>"vanilla"</>"talk.json"

getTalkFriend :: Id -> FilePath
getTalkFriend (Id i) = "talk"</>"friend"</> T.unpack i <> ".json"

getSpellToFriend :: FilePath
getSpellToFriend = "spell"</>"to_friend"</>"vanilla.json"

getSpellLevelUp :: Id -> FilePath
getSpellLevelUp (Id i) = "spell"</>"level_up"</> T.unpack i <> ".json"

getSpellUpgradeRandom :: FilePath
getSpellUpgradeRandom = "spell"</>"upgrade"</>"random.json"

getSpellUpgradeStandard :: FilePath
getSpellUpgradeStandard = "spell"</>"upgrade"</>"srandard.json"

getUpgradeRandom :: FilePath
getUpgradeRandom = "monstergroups"</>"random.json"

getHarvest :: FilePath
getHarvest = "harvest.json"
