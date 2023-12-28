{-# LANGUAGE OverloadedStrings #-}
module Cdda.Spell.Polymorph
  ( spellToFriend
  , spellLevelUp
  , spellUpgradeRandom
  , spellUpgradeStandard
  ) where

import Prelude hiding (id)
import Define.Core
import Define.Spell
import Define.Monster
import Define.MakeFields hiding ( spellToFriend
                                , spellLevelUp
                                , spellUpgradeRandom
                                , spellUpgradeStandard
                                )
import Cdda.Id.Spell
import Cdda.Id.Friend
import Cdda.Id.MonsterGroup

import qualified Data.Text as T
import Data.Default
import Control.Lens

spellToFriend :: Id -> SpellPolymorph
spellToFriend monId@(Id monText) = def
  & id          .~ idSpellToFriend monId
  & name        .~ ("ゾンビ友達化(" <> monText <> ")")
  & description .~ (monText <> "を友達ゾンビレベル1に変化")
  & effectStr   .~ monFriend monId 1

spellLevelUp :: Id -> Int -> SpellPolymorph
spellLevelUp monId@(Id monText) lv = def
  & id          .~ idSpellLevelUp monId lv
  & name        .~ "レベルアップ"
  & description .~ (monText <> "をレベル" <> T.pack (show lv) <> "にする")
  & effectStr   .~ monFriend monId lv

spellUpgradeRandom :: UpgradeRandomType -> Maybe SpellPolymorph
spellUpgradeRandom urt = do
  idSpell <- idSpellUpgradeRandom urt
  idEffect <- randomUpgradeToId urt
  return $ def
    & id          .~ idSpell
    & name        .~ "ランダム進化"
    & description .~ "ゾンビをランダムに進化"
    & effectStr   .~ idEffect

spellUpgradeStandard :: UpgradeStandard -> SpellPolymorph
spellUpgradeStandard us@(UpgradeStandard _ usid@(Id usidText)) = def
  & id          .~ idSpellUpgradeStandard us
  & name        .~ "通常進化"
  & description .~ (usidText <> "に変化する")
  & effectStr   .~ monFriend usid 1
