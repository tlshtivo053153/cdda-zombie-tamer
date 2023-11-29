{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances #-}
module Define.MakeFields where

import Control.Lens

import qualified Define.Effect as E
import Define.EOC
import Define.Item
import Define.Monster
import Define.Recipe
import Define.Talk
import Define.Spell
--import qualified Define.Json as J
import Define.Json ( CddaMod )
import Define.ItemGroup

makeFields ''E.Effect

makeFields ''EOC

makeFields ''Item

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

makeFields ''Spell

makeFields ''CddaMod
makeFields ''ItemGroup
makeFields ''ItemEntry
