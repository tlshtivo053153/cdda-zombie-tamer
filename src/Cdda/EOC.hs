{-# LANGUAGE OverloadedStrings #-}
module Cdda.EOC
  ( npcCastSpell
  , uConsumeItem
  , setStringVar
  , uHasItems
  , npcHasFlag
  , runEocUntil
  , runEocs
  , eocif
  , eocifelse
  , valLevel
  , valNextLevel
  , initLevel
  , hasLevel
  , allEoc
  ) where

import Prelude hiding (id, (++), (==), (<=), (+))
import Define.Core
import Define.EOC
import Define.MakeFields

import Cdda.EOC.Math

import Data.Default
import qualified Data.Text as T

import Control.Lens

showVal :: Val -> String
showVal (UVal val) = "<u_val:" <> T.unpack val <> ">"
showVal (NpcVal val) = "<npc_val:" <> T.unpack val <> ">"
showVal (ContextVal val) = "<context_val:" <> T.unpack val <> ">"
showVal (GlobalVal val) = "<global_val:" <> T.unpack val <> ">"
showVal (VarVal _) = ""

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

runEocUntil :: (ToEValue v) => v -> Id -> Effect
runEocUntil v = RunEocUntil (toEValue v)

runEocs :: (ToEValue v) => v -> Effect
runEocs = construct1 RunEocs

eocif :: (ToEValue v1, ToEValue v2) => v1 -> v2 -> Effect
eocif v1 v2 = If (toEValue v1) (toEValue v2) Nothing

eocifelse :: (ToEValue v1, ToEValue v2, ToEValue v3) => v1 -> v2 -> v3 -> Effect
eocifelse v1 v2 v3 = If (toEValue v1) (toEValue v2) (Just $ toEValue v3)

idInitLevel :: Id
idInitLevel = Id "EOC_ZT_INITIALIZE_LEVEL"

idHasLevel :: Id
idHasLevel = Id "EOC_ZE_UNTIL_HAS_LEVEL"

idConditionLevelLoop :: Id
idConditionLevelLoop = Id "condition_level"

valLoopIndex :: Val
valLoopIndex = UVal "loop_num"

valLoopFlag :: Val
valLoopFlag = ContextVal "loop_flag"

valLoopContinue :: Val
valLoopContinue = UVal "loop_continue"

valLevel :: Val
valLevel = NpcVal "zombie_level"

valNextLevel :: Val
valNextLevel = NpcVal "zombie_next_level"

initLevel :: Eoc
initLevel = def
  & id .~ idInitLevel
  & effect .~
    [ EffectMath $ valLoopIndex =: (1 :: Int)
    , EffectMath $ valLoopContinue =: True
    , EffectMath $ valLevel =: (1 :: Int)
    , SetCondition idConditionLevelLoop $ ConditionAnd
      [ ConditionMath $ Math1 $ valLoopContinue == (1 :: Int)
      , ConditionMath $ Math1 $ valLoopIndex <= (100 :: Int)
      ]
    , runEocUntil idHasLevel idConditionLevelLoop
    , ULoseVar valLoopIndex
    , ULoseVar valLoopContinue
    ]

hasLevel :: Eoc
hasLevel = def
  & id .~ idHasLevel
  & effect .~
    [ setStringVar valLoopFlag (T.pack $ "ZT_LEVEL_" <> showVal valLoopIndex) True
    , eocif (npcHasFlag valLoopFlag)
      [ valLevel =: valLoopIndex
      , valNextLevel =: valLoopIndex + (1 :: Int)
      , valLoopContinue =: (0 :: Int)
      ]
    , EffectMath (mathIncrement valLoopIndex)
    ]

allEoc :: [Eoc]
allEoc =
  [ initLevel
  , hasLevel
  ]
