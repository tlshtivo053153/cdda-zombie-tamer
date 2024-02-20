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
  , valMaxLevel
  , valDodgeMon
  , valSpeedMon
  , valMeleeSkillMon
  , valNeedExpNextLevel
  , valBaseMonster
  , valCanLevelUp
  , initLevel
  , hasLevel
  , initStatus
  , initStatusMonster
  , updateExp
  , canLevelUp
  , allEocLevel
  , allEocStatus
  , allEocExp
  , allEocMonster
  ) where

import Prelude hiding (id, (++), (==), (<=), (+), (<), (>=), (-), (>))
import Define.Core
import Define.EOC
import Define.Monster
import Define.MakeFields

import Cdda.EOC.Math
import Cdda.Monster
import Cdda.Monster.Status
import Cdda.Monster.Exp
import Cdda.Id.MonsterGroup
import Cdda.Id.Monster

import Data.Default
import qualified Data.Text as T
import Data.Maybe

import Control.Lens hiding ( (+=) )

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
idHasLevel = Id "EOC_ZT_UNTIL_HAS_LEVEL"

idConditionLevelLoop :: Id
idConditionLevelLoop = Id "condition_level"

idInitStatus :: Id
idInitStatus = Id "EOC_ZT_INITIALIZE_STATUS"

idInitStatusMonster :: Id -> Id
idInitStatusMonster (Id monId) = Id $ "EOC_ZT_INITIALIZE_STATUS_" <> monId

idUpdateExp :: Id -> Id
idUpdateExp (Id monId) = Id $ "EOC_ZT_UPDATE_EXP_" <> monId

idTotalExp :: Id
idTotalExp = Id "EOC_ZT_TOTAL_EXP"

idExpLoop1 :: Id
idExpLoop1 = Id "EOC_ZT_EXP_LOOP_1"

idExpLoop2 :: Id
idExpLoop2 = Id "EOC_ZT_EXP_LOOP_2"

idConditionExpLoop1 :: Id
idConditionExpLoop1 = Id "condition_exp_loop_1"

idConditionExpLoop2 :: Id
idConditionExpLoop2 = Id "condition_exp_loop_2"

idCanLevelUp :: Id
idCanLevelUp = Id "EOC_CAN_LEVEL_UP"

idCanLevelUpLoop :: Id
idCanLevelUpLoop = Id "EOC_CAN_LEVEL_UP_LOOP"

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

valMaxLevel :: Val
valMaxLevel = NpcVal "zombie_max_level"

valBaseMonster :: Val
valBaseMonster = NpcVal "base_monster_id"

valTmpBaseMonster :: Val
valTmpBaseMonster = ContextVal "base_monster_id"

valBaseMonsterFlag :: Val
valBaseMonsterFlag = ContextVal "base_monster_flag"

valInitStatusEocId :: Val
valInitStatusEocId = ContextVal "init_status_eoc_id"

valUpdateEocId :: Val
valUpdateEocId = ContextVal "update_exp_eoc_id"

valDodge :: Int -> Val
valDodge l = ContextVal $ T.pack $ "dodge" <> show l

valSpeed :: Int -> Val
valSpeed l = ContextVal $ T.pack $ "speed" <> show l

valMeleeSkill :: Int -> Val
valMeleeSkill l = ContextVal $ T.pack $ "melee_skill" <> show l

valExp :: Int -> Val
valExp l = ContextVal $ T.pack $ "exp" <> show (succ l)

valExpTotal :: Val
valExpTotal = undefined

valDodgeMon :: Val
valDodgeMon = NpcVal "zombie_dodge"

valSpeedMon :: Val
valSpeedMon = NpcVal "zombie_speed"

valMeleeSkillMon :: Val
valMeleeSkillMon = NpcVal "zombie_melee_skill"

valTmpStatus :: Val
valTmpStatus = ContextVal "tmp_status"

valTmpStatusV :: Val
valTmpStatusV = VarVal "tmp_status"

valLoopIndex1 :: Val
valLoopIndex1 = UVal "loop_index_1"

valLoopIndex2 :: Val
valLoopIndex2 = UVal "loop_index_2"

valTmpSum :: Val
valTmpSum = ContextVal "sum"

valTmpSumV :: Val
valTmpSumV = VarVal "sum"

valTmpExp :: Val
valTmpExp = ContextVal "tmp_exp"

valTmpExpV :: Val
valTmpExpV = VarVal "tmp_exp"

valTmp :: Val
valTmp = ContextVal "tmp"

valTmpV :: Val
valTmpV = VarVal "tmp"

valTmpTotalExp :: Val
valTmpTotalExp = UVal "tmp_total_exp"

valNeedExpNextLevel :: Val
valNeedExpNextLevel = NpcVal "zombie_need_exp_next_level"

valCurrentExp :: Val
valCurrentExp = NpcVal "zombie_current_exp"

valCanLevelUp :: Val
valCanLevelUp = NpcVal "zombie_can_level_up"

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

initStatus :: Eoc
initStatus = def
  & id .~ idInitStatus
  & effect .~
    [ Foreach "monstergroup" idFriendBase valTmpBaseMonster
      [ setStringVar valBaseMonsterFlag (T.pack $ "ZT_MON_IS_" <> showVal valTmpBaseMonster) True
      , eocif (npcHasFlag valBaseMonsterFlag)
        [ setStringVar valInitStatusEocId (T.pack $ "EOC_ZT_INITIALIZE_STATUS_" <> showVal valTmpBaseMonster) True
        , runEocs valInitStatusEocId
        , setStringVar valUpdateEocId (T.pack $ "EOC_ZT_UPDATE_EXP_" <> showVal valTmpBaseMonster) True
        , runEocs valUpdateEocId
        , setStringVar valBaseMonster (T.pack $ showVal valTmpBaseMonster) True
        ]
      ]
    ]

initStatusMonster :: Id -> Maybe Eoc
initStatusMonster monId = do
  m <- getMonsterFriend monId
  let maxLevel' = m ^. growth.maxLevel
      growth' = m ^. growth
      status' = m ^. status
      statuss = map (\l -> (,) l $ statusWithLevel l growth' status') [1..maxLevel']
  return $ def
    & id .~ idInitStatusMonster monId
    & effect .~ concatMap defineStatus statuss <>
      [ setStringVar valTmpStatus (T.pack $ "_dodge" <> showVal valLevel) True
      , EffectMath $ valDodgeMon =: valTmpStatusV
      , setStringVar valTmpStatus (T.pack $ "_speed" <> showVal valLevel) True
      , EffectMath $ valSpeedMon =: valTmpStatusV
      , setStringVar valTmpStatus (T.pack $ "_melee_skill" <> showVal valLevel) True
      , EffectMath $ valMeleeSkillMon =: valTmpStatusV
      , EffectMath $ valMaxLevel =: maxLevel'
      ]

defineStatus :: (Int, Status) -> [Effect]
defineStatus (l, s) =
  [ EffectMath $ valDodge l =: (s ^. dodge)
  , EffectMath $ valSpeed l =: (s ^. speed)
  , EffectMath $ valMeleeSkill l =: (s ^. melee.skill)
  ]

updateExp :: Id -> Maybe Eoc
updateExp monId = do
  m <- getMonsterFriend monId
  let maxLevel' = m ^. growth.maxLevel
  return $ def
    & id .~ idUpdateExp monId
    & effect .~ map (defineExp m) [1..pred maxLevel']
      <> [ setStringVar valTmpExp (T.pack $ "_exp" <> showVal valNextLevel) True
         , EffectMath $ valNeedExpNextLevel =: valTmpExpV
         ]
      <> defineTotalExp

defineExp :: Monster -> Int -> Effect
defineExp m l = EffectMath $ valExp l =: min 30000 (calcExp m l)

defineTotalExp :: [Effect]
defineTotalExp =
  [ SetCondition idConditionExpLoop1
    $ ConditionMath $ Math1 $ valLoopIndex1 <= valMaxLevel
  , SetCondition idConditionExpLoop2
    $ ConditionMath $ Math1 $ valLoopIndex2 <= valLoopIndex1
  , EffectMath $ valLoopIndex1 =: valNextLevel
  , runEocUntil idExpLoop1 idConditionExpLoop1
  , ULoseVar valLoopIndex1
  , ULoseVar valLoopIndex2
  ]

eocExpLoop1 :: Eoc
eocExpLoop1 = def
  & id .~ idExpLoop1
  & effect .~
    [ EffectMath $ valLoopIndex2 =: valNextLevel
    , setStringVar valTmpSum (T.pack $ "n_exp_total" <> showVal valLoopIndex1) True
    , EffectMath $ valTmpSumV =: (0 :: Int)
    , runEocUntil idExpLoop2 idConditionExpLoop2
    , EffectMath $ mathIncrement valLoopIndex1
    ]

eocExpLoop2 :: Eoc
eocExpLoop2 = def
  & id .~ idExpLoop2
  & effect .~
    [ setStringVar valTmpExp (T.pack $ "_exp" <> showVal valLoopIndex2) True
    , EffectMath $ valTmpSumV += valTmpExpV
    , EffectMath $ mathIncrement valLoopIndex2
    ]

canLevelUp :: Eoc
canLevelUp = def
  & id .~ idCanLevelUp
  & effect .~
    [ EffectMath $ valLoopIndex =: valNextLevel
    , setStringVar valTmp (T.pack $ "n_exp_total" <> showVal valLoopIndex) True
    , EffectMath $ valTmpTotalExp =: valTmpV
    , EffectMath $ valLoopContinue =: True
    , SetCondition idConditionLevelLoop $ ConditionMath $ Math1 $ valLoopContinue == True
    , runEocUntil idCanLevelUpLoop idConditionLevelLoop
    , ULoseVar valTmpTotalExp
    , ULoseVar valLoopIndex
    , ULoseVar valLoopContinue
    ]

canLevelUpLoop :: Eoc
canLevelUpLoop = def
  & id .~ idCanLevelUpLoop
  & effect .~
    [ eocif (ConditionMath $ Math1 $ valLoopIndex == valMaxLevel)
        [ EffectMath $ valLoopContinue =: False
        , EffectMath $ valCanLevelUp =: valLoopIndex
        ]
    , eocif (ConditionMath $ Math1 $ valTmpTotalExp > valCurrentExp)
        [ EffectMath $ valLoopContinue =: False
        , EffectMath $ valCanLevelUp =: valLoopIndex - (1 :: Int)
        ]
    , EffectMath $ mathIncrement valLoopIndex
    , setStringVar valTmp (T.pack $ "n_exp_total" <> showVal valLoopIndex) True
    , EffectMath $ valTmpTotalExp =: valTmpV
    ]

allEocLevel :: [Eoc]
allEocLevel =
  [ initLevel
  , hasLevel
  ]

allEocStatus :: [Eoc]
allEocStatus = [ initStatus ]

allEocExp :: [Eoc]
allEocExp =
  [ eocExpLoop1
  , eocExpLoop2
  , canLevelUp
  , canLevelUpLoop
  ]

allEocMonster :: [Eoc]
allEocMonster =
  mapMaybe initStatusMonster allFriendMonster
  <> mapMaybe updateExp allFriendMonster
