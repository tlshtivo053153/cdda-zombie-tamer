module Cdda.EOC
  ( npcCastSpell
  , uConsumeItem
  , setStringVar
  , uHasItems
  , npcHasFlag
  ) where

import Define.EOC

construct1 :: (ToEValue v) => (EValue -> Effect) -> v -> Effect
construct1 f v = f (toEValue v)

construct2 :: (ToEValue v1, ToEValue v2) => (EValue -> EValue -> Effect) -> v1 -> v2 -> Effect
construct2 f v1 v2 = f (toEValue v1) (toEValue v2)

construct3 :: (ToEValue v1, ToEValue v2, ToEValue v3) =>
              (EValue -> EValue -> EValue -> Effect) -> v1 -> v2 -> v3 -> Effect
construct3 f v1 v2 v3 = f (toEValue v1) (toEValue v2) (toEValue v3)

npcCastSpell :: (ToEValue v1, ToEValue v2) => v1 -> v2 -> Effect
npcCastSpell = construct2 NpcCastSpell

uConsumeItem :: (ToEValue v1, ToEValue v2) => v1 -> v2 -> Effect
uConsumeItem = construct2 UConsumeItem

setStringVar :: (ToEValue v1, ToEValue v2, ToEValue v3) => v1 -> v2 -> v3 -> Effect
setStringVar = construct3 SetStringVar

uHasItems :: (ToEValue v1, ToEValue v2) => v1 -> v2 -> Effect
uHasItems = construct2 UHasItems

npcHasFlag :: ToEValue v => v -> Effect
npcHasFlag = construct1 NpcHasFlag
