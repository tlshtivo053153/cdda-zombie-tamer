{-# LANGUAGE OverloadedStrings #-}
module Cdda.Spell.DeathFunction
  ( spellDeathOverride
  ) where

import Prelude hiding (id)

import Define.Core
import Define.Spell
import Define.MakeFields

import Cdda.Id.Spell

import Data.Default
import Control.Lens

spellDeathOverride :: Id -> Id -> Id -> SpellDeathFunctionOverride
spellDeathOverride monId deathId extendSpellId = def
  & id          .~ idSpellOverrideDeathFunction monId
  & name        .~ "death_functionの追加"
  & description .~ ""
  & effects     .~ [deathId, extendSpellId]
