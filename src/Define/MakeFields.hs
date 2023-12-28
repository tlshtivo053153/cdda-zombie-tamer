{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances #-}
module Define.MakeFields
  ( module Define.MakeFields
  ) where

import Control.Lens

import qualified Define.Effect as E
import Define.EOC
import Define.Item.Petfood
import Define.Monster
import Define.Recipe
import Define.Talk
import Define.Spell
import Define.Json ( CddaMod )
import Define.ItemGroup
import Define.HarvestDropType
import Define.TerFurnTransform
import Define.Furniture
import Define.DeathFunction

makeFields ''E.Effect

makeFields ''EOC

makeFields ''ItemPetfood

makeFields ''Armor
makeFields ''Damage
makeFields ''Melee
makeFields ''Status
makeFields ''ArmorGrowth
makeFields ''DamageGrowth
makeFields ''MeleeGrowth
makeFields ''StatusGrowth
makeFields ''Monster

makeFields ''Recipe

makeFields ''TResponse
makeFields ''Response
makeFields ''Talk
makeFields ''TalkConfig

makeFields ''SpellPolymorph
makeFields ''SpellTerTransform
makeFields ''SpellDeathFunctionOverride

makeFields ''CddaMod
makeFields ''ItemGroup
makeFields ''ItemEntry

makeFields ''HarvestDropType

makeFields ''TerFurnTransform
makeFields ''TransFurniture

makeFields ''Furniture

makeFields ''DeathFunction
