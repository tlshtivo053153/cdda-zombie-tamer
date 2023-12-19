{-# LANGUAGE OverloadedStrings #-}
module Cdda.Spell.DeathFunction where

import Define.Core
import Define.Spell

import Cdda.Id.Spell

spellDeathOverride :: Id -> Id -> Id -> SpellDeathFunctionOverride
spellDeathOverride monId deathId extendSpellId = SpellDeathFunctionOverride
  { _spellDeathFunctionOverrideId          = idSpellOverrideDeathFunction monId
  , _spellDeathFunctionOverrideName        = ""
  , _spellDeathFunctionOverrideDescription = ""
  , _spellDeathFunctionOverrideEffects     = [deathId, extendSpellId]
  }
