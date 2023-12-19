{-# LANGUAGE OverloadedStrings #-}
module Cdda.Spell.Polymorph
  ( spellToFriend
  , spellLevelUp
  , spellUpgradeRandom
  , spellUpgradeStandard
  , spellPlaceMeatSlime
  , spellPlaceMarrowSlime
  ) where

import Define.Core
import Define.Spell
import Define.Monster

import Cdda.Id.Spell
import Cdda.Id.Friend
import Cdda.Id.MonsterGroup
import Cdda.Id.TerFurnTransform

import qualified Data.Text as T

spellToFriend :: Id -> Spell
spellToFriend monId@(Id monText) =
  Spell (idSpellToFriend monId)
        ("ゾンビ友達化(" <> monText <> ")")
        (monText <> "を友達ゾンビレベル1に変化")
        $ monFriend monId 1

spellLevelUp :: Id -> Int -> Spell
spellLevelUp monId@(Id monText) lv =
  Spell (idSpellLevelUp monId lv)
        "レベルアップ"
        (monText <> "をレベル" <> T.pack (show lv) <> "にする")
        $ monFriend monId lv

spellUpgradeRandom :: UpgradeRandomType -> Maybe Spell
spellUpgradeRandom urt =
  Spell <$> idSpellUpgradeRandom urt
        <*> Just "ランダム進化"
        <*> Just "ゾンビをランダムに進化"
        <*> randomUpgradeToId urt

spellUpgradeStandard :: UpgradeStandard -> Spell
spellUpgradeStandard us@(UpgradeStandard _ usid@(Id usidText)) =
  Spell (idSpellUpgradeStandard us)
        "通常進化"
        (usidText <> "に変化する")
        $ monFriend usid 1

spellPlaceMeatSlime :: Strength -> Spell
spellPlaceMeatSlime s@(Strength n) =
  Spell (idSpellPlaceMeatSlime s)
        "肉スライムを配置"
        ("肉スライム" <> T.pack (show n) <> "を配置")
        (idTransPlaceMeatSlime s)

spellPlaceMarrowSlime :: Strength -> Spell
spellPlaceMarrowSlime s@(Strength n) =
  Spell (idSpellPlaceMarrowSlime s)
        "骨髄スライムを配置"
        ("骨髄スライム" <> T.pack (show n) <> "を配置")
        (idTransPlaceMarrowSlime s)
