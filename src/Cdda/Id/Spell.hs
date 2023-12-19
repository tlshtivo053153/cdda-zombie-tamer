{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.Spell
  ( idSpellToFriend
  , idSpellLevelUp
  , idSpellUpgradeRandom
  , idSpellUpgradeStandard
  , idSpellPlaceMeatSlime
  , idSpellPlaceMarrowSlime
  , idSpellDeathGasZombie
  , idSpellDeathChildZombie
  , idSpellDeathSmokerZombie
  , idSpellDeathZombieHollow
  , idSpellDeathNecroBoomer
  , idSpellDeathZombieGasbag
  , idSpellDeathBoomerHuge
  , idSpellDeathZombieRelaxGasbag
  , idSpellDeathZombieTearGasbag
  , idSpellDeathDevourerLabSec
  , idSpellDeathZombieChildScorched
  , idSpellDeathZombieAcidic
  , idSpellDeathSpawnRaptorUnstable
  , idSpellOverrideDeathFunction
  ) where

import Define.Core
import Define.Talk
import Define.Monster

import Cdda.Id.MonsterGroup

import qualified Data.Text as T

idSpellToFriend :: Id -> Id
idSpellToFriend (Id monId) = Id $ "spell_" <> monId <> "_to_friend"

idSpellLevelUp :: Id -> Int -> Id
idSpellLevelUp (Id monId) lv = Id $ "spell_" <> monId <> "_level_" <> T.pack (show lv)

idSpellUpgradeRandom :: UpgradeRandomType -> Maybe Id
idSpellUpgradeRandom urt =
  let groupId = randomUpgradeToId urt
   in case groupId of
        Just (Id groupText) -> Just $
          Id $ "spell_random_upgrade_" <> T.toLower groupText
        Nothing -> Nothing

idSpellUpgradeStandard :: UpgradeStandard -> Id
idSpellUpgradeStandard (UpgradeStandard _ (Id usidText)) =
  Id $ "spell_standard_upgrade_" <> usidText

idSpellPlaceMeatSlime :: Strength -> Id
idSpellPlaceMeatSlime (Strength n) = Id $ "spell_place_meat_slime_" <> T.pack (show n)

idSpellPlaceMarrowSlime :: Strength -> Id
idSpellPlaceMarrowSlime (Strength n) = Id $ "spell_place_marrow_slime_" <> T.pack (show n)

idSpellDeathGasZombie :: Id
idSpellDeathGasZombie = Id "death_conflagration"

idSpellDeathChildZombie :: Id
idSpellDeathChildZombie = Id "death_guilt"

idSpellDeathSmokerZombie :: Id
idSpellDeathSmokerZombie = Id "death_smokeburst"

idSpellDeathZombieHollow :: Id
idSpellDeathZombieHollow = Id "death_blobsplit"

idSpellDeathNecroBoomer :: Id
idSpellDeathNecroBoomer = Id "necro_boomer_death"

idSpellDeathZombieGasbag :: Id
idSpellDeathZombieGasbag = Id "death_gas"

idSpellDeathBoomerHuge :: Id
idSpellDeathBoomerHuge = Id "death_boomer_glow"

idSpellDeathZombieRelaxGasbag :: Id
idSpellDeathZombieRelaxGasbag = Id "death_relax_gas"

idSpellDeathZombieTearGasbag :: Id
idSpellDeathZombieTearGasbag = Id "death_tearburst"

idSpellDeathDevourerLabSec :: Id
idSpellDeathDevourerLabSec = Id "death_portal"

idSpellDeathZombieChildScorched :: Id
idSpellDeathZombieChildScorched = Id "death_zombie_kinderling"

idSpellDeathZombieAcidic :: Id
idSpellDeathZombieAcidic = Id "death_acid"

idSpellDeathSpawnRaptorUnstable :: Id
idSpellDeathSpawnRaptorUnstable = Id "death_explosion_mon_spawn_raptor_unstable"

idSpellOverrideDeathFunction :: Id -> Id
idSpellOverrideDeathFunction (Id monId) = Id $ "spell_df_override_" <> monId
