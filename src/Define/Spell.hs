module Define.Spell where

import Define.Core

import Data.Text (Text)

data Spell = Spell
  { _spellId :: Id
  , _spellName :: Text
  , _spellDescription :: Text
  , _spellEffectStr :: Id
  }
