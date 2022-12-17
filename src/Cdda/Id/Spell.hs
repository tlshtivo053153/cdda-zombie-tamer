{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.Spell
  ( idSpellToFriend
  , idSpellLevelUp
  , idSpellUpgradeRandom
  , idSpellUpgradeStandard
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

