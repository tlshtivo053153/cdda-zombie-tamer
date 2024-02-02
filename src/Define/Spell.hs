{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Define.Spell
  ( SpellPolymorph(..)
  , SpellTerTransform
  , SpellDeathFunctionOverride(..)
  ) where

import Define.Core

import GHC.Generics (Generic)
import Data.Default

import Data.Text (Text)

data SpellPolymorph = SpellPolymorph
  { _spellPolymorphId :: Id
  , _spellPolymorphName :: Text
  , _spellPolymorphDescription :: Text
  , _spellPolymorphEffectStr :: Id
  }
  deriving (Generic, Default)

data SpellTerTransform = SpellTerTransform
  { _spellTerTransformId :: Id
  , _spellTerTransformName :: Text
  , _spellTerTransformDescription :: Text
  , _spellTerTransformEffectStr :: Id
  }
  deriving (Generic, Default)

data SpellDeathFunctionOverride = SpellDeathFunctionOverride
  { _spellDeathFunctionOverrideId :: Id
  , _spellDeathFunctionOverrideName :: Text
  , _spellDeathFunctionOverrideDescription :: Text
  , _spellDeathFunctionOverrideEffects :: [Id]
  }
  deriving (Generic, Default)
