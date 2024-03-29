module Cdda.FilePath
  ( getModInfo
  , getItemFood
  , getMonsterVanillaFriend
  , getMonsterVanillaNonFriend
  , getMonsterFriend
  , getTalkVanilla
  , getTalkFriend
  , getSpellToFriend
  , getSpellLevelUp
  , getSpellUpgradeRandom
  , getSpellUpgradeStandard
  , getSpellDeathFunc
  , getSpellDeathFuncOverride
  , getUpgradeRandom
  , getFriendGroup
  , getHarvest
  , getItemGroup
  , getHarvestDropType
  , getFurniture
  , getTerFurnTransform
  , getFlag
  , getEocLevel
  , getEocStatus
  , getEocExp
  , getEocMonster
  ) where

import System.FilePath
import qualified Data.Text as T

import Define.Core

getModInfo :: FilePath
getModInfo = "modinfo.json"

getItemFood :: FilePath
getItemFood = "items"</>"petfood.json"

getMonsterVanillaFriend :: FilePath
getMonsterVanillaFriend = "monster"</>"vanilla"</>"friend.json"

getMonsterVanillaNonFriend :: FilePath
getMonsterVanillaNonFriend = "monster"</>"vanilla"</>"non_friend.json"

getMonsterFriend :: Id -> FilePath
getMonsterFriend (Id i) = "monster"</>"friend"</> T.unpack i <> ".json"

getTalkVanilla :: FilePath
getTalkVanilla = "talk"</>"vanilla.json"

getTalkFriend :: FilePath
getTalkFriend = "talk"</>"friend.json"

getSpellToFriend :: FilePath
getSpellToFriend = "spell"</>"to_friend"</>"vanilla.json"

getSpellLevelUp :: Id -> FilePath
getSpellLevelUp (Id i) = "spell"</>"level_up"</> T.unpack i <> ".json"

getSpellUpgradeRandom :: FilePath
getSpellUpgradeRandom = "spell"</>"upgrade"</>"random.json"

getSpellUpgradeStandard :: FilePath
getSpellUpgradeStandard = "spell"</>"upgrade"</>"srandard.json"

getSpellDeathFunc :: FilePath
getSpellDeathFunc = "spell"</>"death_function.json"

getSpellDeathFuncOverride :: FilePath
getSpellDeathFuncOverride = "spell"</>"death_function_override.json"

getUpgradeRandom :: FilePath
getUpgradeRandom = "monstergroups"</>"random.json"

getFriendGroup :: FilePath
getFriendGroup = "monstergroups"</>"friends.json"

getHarvest :: FilePath
getHarvest = "harvest.json"

getItemGroup :: FilePath
getItemGroup = "item_group.json"

getHarvestDropType :: FilePath
getHarvestDropType = "harvest_drop_type.json"

getFurniture :: FilePath
getFurniture = "furniture.json"

getTerFurnTransform :: FilePath
getTerFurnTransform = "ter_furn_transform.json"

getFlag :: FilePath
getFlag = "flags.json"

getEocLevel :: FilePath
getEocLevel = "eoc"</>"level.json"

getEocStatus :: FilePath
getEocStatus = "eoc"</>"status.json"

getEocExp :: FilePath
getEocExp = "eoc"</>"exp.json"

getEocMonster :: Id -> FilePath
getEocMonster (Id monId) = "eoc"</>"monster"</> T.unpack monId <> ".json"
