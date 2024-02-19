{-# LANGUAGE OverloadedStrings #-}
module Cdda.Talk.Friend
  ( friendTalk
  ) where

import Prelude hiding (id, (+), (-), (*), (>), (>=), (++), (==), (<))

import Define.Core
import Define.Monster
import Define.Talk
import Define.MakeFields
import Define.EOC
import Define.Flag

import Cdda.Talk.Utils
import Cdda.Talk.Config

import qualified Cdda.Monster as M
import Cdda.Id.Item
import Cdda.Id.Spell
import Cdda.Monster.Exp
import Cdda.Monster.Upgrade
import Cdda.EOC.Math
import Cdda.EOC

import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe
import Data.Default
import Control.Monad.Reader

import Control.Lens hiding ( (+=), (-=) )

idMain :: Id
idMain = Id "MAIN"

idItemFed :: Id
idItemFed = Id "ITEM_FED"

idPreItemFed :: Id
idPreItemFed = Id "PRE_ITEM_FED"

idFeedItem :: Id
idFeedItem = Id "FEED_ITEM"

idNotFeedItem :: Id
idNotFeedItem = Id "NOT_FEED_ITEM"

idFeed :: Id
idFeed = Id "FEED"

idUpgradeDone :: Id
idUpgradeDone = Id "UPGRADE_DONE"

idUpgradeRandomMonster :: Id
idUpgradeRandomMonster = Id "UPGRADE_RANDOM_MONSTER"

idUpgradeRandom :: Id
idUpgradeRandom = Id "UPGRADE_RANDOM"

idUpgradeStandardMonster :: Id
idUpgradeStandardMonster = Id "UPGRADE_STANDARD_MONSTER_"

idUpgradeStandard :: Id
idUpgradeStandard = Id "UPGRADE_STANDARD"

idLevelUpDone :: Id
idLevelUpDone = Id "LEVEL_UP_DONE"

idLevelUp :: Id
idLevelUp = Id "LEVEL_UP"

idSelectedLevel :: Id
idSelectedLevel = Id "SELECTED_LEVEL"

idShowStatus :: Id
idShowStatus = Id "SHOW_STATUS"

valCurrentExp :: Val
valTotalExp :: Val
valTmpCurrentExp :: Val
valTmpTotalExp :: Val
valTmpNeedExp :: Val
valTmpHp :: Val
valTmpMaxHp :: Val
valTmpBash :: Val
valTmpBullet :: Val
valTmpCut :: Val
valTmpStab :: Val
valTmpDodge :: Val
valTmpSpeed :: Val
valTmpMeleeSkill :: Val
valTmpFoodId :: Val
valTmpFoodName :: Val
valTmpFoodHaveNum :: Val
valTmpFoodConsumeNum :: Val
valTmpFoodExp :: Val
valTmpLevel :: Val
valTmpNextLevel :: Val
valNeedItemId :: Val
valNeedItemNum :: Val
valSpellUpgrade :: Val
valIsInitialize :: Val
valCanIncrease :: Val
valCanIncreased :: Val
valSelectLevel :: Val
valConsumeExp :: Val
valTmpTotal :: Val
valTmpTotalV :: Val
valSpellLevelUp :: Val
valGetExp :: Val

valCurrentExp = NpcVal "zombie_current_exp"
valTotalExp = NpcVal "zombie_total_exp"
valTmpCurrentExp = ContextVal "tmp_zombie_current_exp"
valTmpTotalExp = ContextVal "tmp_zombie_total_exp"
valTmpNeedExp = ContextVal "tmp_zombie_need_exp"
valTmpHp = ContextVal "tmp_zombie_hp"
valTmpMaxHp = ContextVal "tmp_zombie_max_hp"
valTmpBash = ContextVal "tmp_zombie_bash"
valTmpBullet = ContextVal "tmp_zombie_bullet"
valTmpCut = ContextVal "tmp_zombie_cut"
valTmpStab = ContextVal "tmp_zombie_stab"
valTmpDodge = ContextVal "tmp_zombie_dodge"
valTmpSpeed = ContextVal "tmp_zombie_speed"
valTmpMeleeSkill = ContextVal "tmp_zombie_melee"
valTmpFoodId = ContextVal "tmp_food_id"
valTmpFoodName = ContextVal "tmp_food_name"
valTmpFoodHaveNum = ContextVal "tmp_food_have_num"
valTmpFoodConsumeNum = ContextVal "tmp_food_consume_num"
valTmpFoodExp = ContextVal "tmp_food_exp"
valTmpLevel = ContextVal "tmp_level"
valTmpNextLevel = ContextVal "tmp_next_level"
valNeedItemId = ContextVal "need_item_id"
valNeedItemNum = ContextVal "need_item_num"
valSpellUpgrade = ContextVal "spell_upgrade"
valIsInitialize = NpcVal "is_initialize"
valCanIncrease = ContextVal "can_increase"
valCanIncreased = ContextVal "can_incresed"
valSelectLevel = ContextVal "select_level"
valConsumeExp = ContextVal "consume_exp"
valTmpTotal = ContextVal "tmp_total_exp"
valTmpTotalV = VarVal "tmp_total_exp"
valSpellLevelUp = ContextVal "spell_level_up"
valGetExp = ContextVal "get_exp"

showVal :: Val -> String
showVal (UVal val) = "<u_val:" <> T.unpack val <> ">"
showVal (NpcVal val) = "<npc_val:" <> T.unpack val <> ">"
showVal (ContextVal val) = "<context_val:" <> T.unpack val <> ">"
showVal (GlobalVal val) = "<global_val:" <> T.unpack val <> ">"
showVal (VarVal _) = ""

showItemName :: Id -> String
showItemName (Id itemName) = "<item_name:" <> T.unpack itemName <> ">"

initVar :: [Effect]
initVar =
  [ runEocs [ initLevel ^. id
            , initStatus ^. id
            ]
  , NpcAddVar valIsInitialize "yes"
  ]

upgradeRandomToRespose :: (UpgradeRandom, [Flag]) -> Response
upgradeRandomToRespose (UpgradeRandom (UCHaveItem itemId n) urt, fs) =
  let resText = T.pack $ concat [ showItemName itemId, " ", show n]
      spell = runId <$> idSpellUpgradeRandom urt
   in simpleResponse resText talkUpgradeRandomMonster
    & condition .~ case fs of
                     [Flag f] -> ConditionEffect $ npcHasFlag f
                     _ -> ConditionOr $ map (ConditionEffect . npcHasFlag . runFlag) fs
    & successEffect .~ catMaybes
      [ Just $ setStringVar valNeedItemId itemId False
      , Just $ EffectMath $ valNeedItemNum =: n
      , setStringVar valSpellUpgrade <$> spell <*> return False
      ]

upgradeStandardToRespose :: (UpgradeStandard, [Flag]) -> Response
upgradeStandardToRespose (us@(UpgradeStandard (UCHaveItem itemId n) monId), fs) = do
  let resText = case M.idToName monId of
                  Just monName -> T.pack $ concat [ "[", showItemName itemId, " ", show n, "]"
                                , " ", T.unpack monName ]
                  Nothing -> "nothing"
      spell = idSpellUpgradeStandard us
   in simpleResponse resText talkUpgradeStandardMonster
    & condition .~ case fs of
                     [Flag f] -> ConditionEffect $ npcHasFlag f
                     _ -> ConditionOr $ map (ConditionEffect . npcHasFlag . runFlag) fs
    & successEffect .~
      [ setStringVar valNeedItemId itemId False
      , EffectMath $ valNeedItemNum =: n
      , setStringVar valSpellUpgrade spell False
      ]

responseMainFeed :: Response
responseMainFeed = simpleResponse "餌を与える" talkFeed

responseMainUpgradeRandom :: Response
responseMainUpgradeRandom = simpleResponse "ランダム進化" talkUpgradeRandomMain

responseMainUpgradeStandard :: Response
responseMainUpgradeStandard = simpleResponse "通常進化" talkUpgradeStandard

responseMainLevelUp :: Response
responseMainLevelUp =
  simpleResponse "レベルアップ可能" talkLevelUp
    & condition .~ ConditionAnd
      [ ConditionMath (Math1 $ valCurrentExp >= valNeedExpNextLevel)
      , ConditionMath (Math1 $ valLevel < valMaxLevel)
      ]
    & successEffect .~
      [ EffectMath $ valTmpLevel =: valLevel
      , EffectMath $ valTmpNextLevel =: valNextLevel
      , EffectMath $ valTmpCurrentExp =: valCurrentExp
      , runEocs (canLevelUp ^. id)
      , EffectMath $ valCanIncrease =: valCanLevelUp - valLevel
      , EffectMath $ valCanIncreased =: valCanLevelUp
      ]

responseMainSpecialAction :: (Maybe Response)
responseMainSpecialAction = Nothing

responseMainShowStatus :: Response
responseMainShowStatus =
  simpleResponse "ステータス" talkShowStatus
    & successEffect .~
      [ EffectMath $ valTmpCurrentExp =: valCurrentExp
      , EffectMath $ valTmpTotalExp =: valTotalExp
      , EffectMath $ valTmpLevel =: valLevel
      , EffectMath $ valTmpHp =: MathExpr "n_hp('torso')"
      , EffectMath $ valTmpMaxHp =: MathExpr "n_hp_max('torso')"
      , EffectMath $ valTmpBash =: MathExpr "n_armor('bash', 'torso')"
      , EffectMath $ valTmpBullet =: MathExpr "n_armor('bullet', 'torso')"
      , EffectMath $ valTmpCut =: MathExpr "n_armor('cut', 'torso')"
      , EffectMath $ valTmpStab =: MathExpr "n_armor('stab', 'torso')"
      , EffectMath $ valTmpDodge =: valDodgeMon
      , EffectMath $ valTmpSpeed =: valSpeedMon
      , EffectMath $ valTmpMeleeSkill =: valMeleeSkillMon
      ]

talkMain :: Talk
talkMain =
  let res =
        [ responseMainFeed
        , responseMainUpgradeRandom
        , responseMainUpgradeStandard
        , responseMainLevelUp
        , responseMainShowStatus
        , responseDone
        ]
   in returnTalk idMain $ def
    & dynamicLine .~ DynamicLineText "友達ゾンビに話しかけたときのフーレバー"
    & responses .~ res
    & speakerEffectCondition ?~ ConditionNot (ConditionEffect $ NpcHasVar valIsInitialize "yes")
    & speakerEffect .~ initVar

talkItemFed :: Talk
talkItemFed =
  returnTalk idItemFed $ def
    & dynamicLine .~ DynamicLineText "よろこんでいるように見えます。"
    & responses .~ [ responseTop ]

talkFeedItem :: Talk
talkFeedItem =
  let dlText = T.pack $ unlines [ "選択アイテム: " <> showVal valTmpFoodName
                                , "所持数: " <> showVal valTmpFoodHaveNum
                                , "増加経験値: " <> showVal valTmpFoodExp
                                ]
   in returnTalk idFeedItem $ def
    & dynamicLine .~ DynamicLineText dlText
    & responses .~ responseFeedItem <> [ responseBack ]

responseFeedItem :: [Response]
responseFeedItem =
    [ simpleResponse "1個使用" talkPreItemFed
      & successEffect .~
        [ EffectMath $ valTmpFoodConsumeNum =: (1 :: Int)
        , EffectMath $ valGetExp =: valTmpFoodExp
        ]
    , simpleResponse "すべて使用" talkPreItemFed
      & successEffect .~
        [ EffectMath $ valTmpFoodConsumeNum =: valTmpFoodHaveNum
        , EffectMath $ valGetExp =: valTmpFoodHaveNum * valTmpFoodExp
        ]
    , simpleResponse "個数を選択" talkPreItemFed
      & successEffect .~
        [ EffectMath $ valTmpFoodConsumeNum =: MathExpr "num_input('個数を入力', 0)"
        , EffectMath $ valTmpFoodConsumeNum =:
            mathFunc3 "clamp" valTmpFoodConsumeNum (0 :: Int) valTmpFoodHaveNum
        , EffectMath $ valGetExp =: valTmpFoodConsumeNum * valTmpFoodExp
        ]
    ]

talkNotFeedItem :: Talk
talkNotFeedItem =
  returnTalk idNotFeedItem $ def
    & dynamicLine .~ DynamicLineText "アイテムを持っていない"
    & responses .~ [ responseBack ]

responseFeed :: [Response]
responseFeed = map responseFeed'
  [ idTaintedMeatPremium
  , idTaintedMeatHighPremium
  , idTaintedMarrowPremium
  , idTaintedMarrowHighPremium
  ]

responseFeed' :: Id -> Response
responseFeed' itemId =
  let itemText = T.pack $ showItemName itemId
   in simpleResponse itemText talkFeedItem
    & trial .~ ConditionEffect (uHasItems itemId (1 :: Int))
    & successEffect .~
      [ setStringVar valTmpFoodId itemId False
      , setStringVar valTmpFoodName (T.pack $ showItemName itemId) True
      , EffectMath $ valTmpFoodExp =: itemExp itemId
      , EffectMath $ valTmpFoodHaveNum =: mathFunc1 "u_item_count"
                                                    (MathExpr $ "'" <> runId itemId <> "'")
      ]
    & failure ?~ talkNotFeedItem

talkFeed :: Talk
talkFeed =
  returnTalk idFeed $ def
    & dynamicLine .~ DynamicLineText "餌の種類を選択"
    & responses .~ responseFeed <> [ responseBack ]

talkPreItemFed :: Talk
talkPreItemFed =
  let dlText = T.pack $ unlines [ "使用個数: " <> showVal valTmpFoodConsumeNum
                                , "経験値: " <> showVal valGetExp
                                ]
   in returnTalk idPreItemFed $ def
    & dynamicLine .~ DynamicLineText dlText
    & responses .~ responsePreItemFed : [ responseBack ]

responsePreItemFed :: Response
responsePreItemFed =
  simpleResponse "餌を与える" talkItemFed
    & condition .~ ConditionMath (Math1 $ valTmpFoodConsumeNum > (0 :: Int))
    & successEffect .~
      [ uConsumeItem valTmpFoodId valTmpFoodConsumeNum
      , EffectMath $ valCurrentExp += valGetExp
      , EffectMath $ valTotalExp += valGetExp
      ]

talkUpgradeDone :: Talk
talkUpgradeDone =
  returnTalk idUpgradeDone $ def
    & dynamicLine .~ DynamicLineText "全身が痙攣を起こして喜んでいるように見えます"
    & responses .~ [ responseDone ]

talkUpgradeRandomMonster :: Talk
talkUpgradeRandomMonster =
  returnTalk idUpgradeRandomMonster $ def
    & dynamicLine .~ DynamicLineText "(口を開けている)"
    & responses .~ [ responseUpgradeRandomMonster
                   , responseBack
                   ]

responseUpgradeRandomMonster :: Response
responseUpgradeRandomMonster = do
  simpleResponse "口の中に餌を入れる" talkUpgradeDone
    & condition .~ ConditionEffect (uHasItems valNeedItemId valNeedItemNum)
    & successEffect .~
      [ npcCastSpell valSpellUpgrade False
      , uConsumeItem valNeedItemId valNeedItemNum
      , EffectMath $ valCurrentExp =: valTotalExp
      , EffectMath $ valLevel =: (1 :: Int)
      , EffectMath $ valNextLevel =: (2 :: Int)
      , runEocs (initStatus ^. id)
      ]

talkUpgradeRandomMain :: Talk
talkUpgradeRandomMain =
  returnTalk idUpgradeRandom $ def
    & dynamicLine .~ DynamicLineText "ランダム進化メニュー"
    & responses .~ responseUpgradeRandomMain <> [responseBack]

responseUpgradeRandomMain :: [Response]
responseUpgradeRandomMain = map upgradeRandomToRespose allUpgradeRandomList

talkUpgradeStandardMonster :: Talk
talkUpgradeStandardMonster =
  returnTalk idUpgradeStandardMonster $ def
    & dynamicLine .~ DynamicLineText "(口を開けている)"
    & responses .~ [ responseUpgradeStandardMonster
                   , responseBack
                   ]

responseUpgradeStandardMonster :: Response
responseUpgradeStandardMonster =
  simpleResponse "口の中に餌を入れる" talkUpgradeDone
    & condition .~ ConditionEffect (uHasItems valNeedItemId valNeedItemNum)
    & successEffect .~
      [ npcCastSpell valSpellUpgrade False
      , uConsumeItem valNeedItemId valNeedItemNum
      , EffectMath $ valCurrentExp =: valTotalExp
      , EffectMath $ valLevel =: (1 :: Int)
      , EffectMath $ valNextLevel =: (2 :: Int)
      , runEocs (initStatus ^. id)
      ]

talkUpgradeStandard :: Talk
talkUpgradeStandard =
  returnTalk idUpgradeStandard $ def
    & dynamicLine .~ DynamicLineText "通常進化メニュー"
    & responses .~ responseUpgradeStandard <> [ responseBack ]

responseUpgradeStandard :: [Response]
responseUpgradeStandard = map upgradeStandardToRespose allUpgradeStandardList

talkLevelUpDone :: Talk
talkLevelUpDone = do
  returnTalk idLevelUpDone $ def
    & dynamicLine .~ DynamicLineText "レベルが上がりました"
    & responses .~ [ responseDone ]

talkLevelUp :: Talk
talkLevelUp =
  returnTalk idLevelUp $ def
    & dynamicLine .~ DynamicLineText (T.pack $ "レベルアップメニュー(現在レベル" <> showVal valTmpLevel <> ")")
    & responses .~ responseLevelUp <> [ responseBack ]

responseLevelUp :: [Response]
responseLevelUp =
    [ simpleResponse (T.pack $ "[レベル+1] レベル" <> showVal valTmpNextLevel) talkSelectedLevel
      & successEffect .~
        [ EffectMath $ valSelectLevel =: valNextLevel
        , EffectMath $ valConsumeExp =: valNeedExpNextLevel
        ]
    , simpleResponse (T.pack $ "[レベル+" <> showVal valCanIncrease <> "] レベル" <> showVal valCanIncreased) talkSelectedLevel
      & successEffect .~
        [ EffectMath $ valSelectLevel =: valCanIncreased
        , setStringVar valTmpTotal (T.pack $ "n_exp_total" <> showVal valSelectLevel) True
        , EffectMath $ valConsumeExp =: valTmpTotalV
        ]
    , simpleResponse "変化後のレベルを選択" talkSelectedLevel
      & successEffect .~
        [ EffectMath $ valSelectLevel =: MathExpr "num_input('変化後のレベルを入力', 0)"
        , EffectMath $ valSelectLevel =:
            mathFunc3 "clamp" valSelectLevel valNextLevel valCanIncreased
        , setStringVar valTmpTotal (T.pack $ "n_exp_total" <> showVal valSelectLevel) True
        , EffectMath $ valConsumeExp =: valTmpTotalV
        ]
    ]

talkSelectedLevel :: Talk
talkSelectedLevel =
  returnTalk idSelectedLevel $ def
    & dynamicLine .~ DynamicLineText (T.pack $ unlines
                                      [ "現在レベル: " <> showVal valTmpLevel
                                      <> "、変化後のレベル: " <> showVal valSelectLevel
                                      , "消費経験値: " <> showVal valConsumeExp
                                      <> "、現在経験値: " <> showVal valTmpCurrentExp
                                      ]
                                     )
    & responses .~ responseSelectedLevel : [responseBack]

responseSelectedLevel :: Response
responseSelectedLevel =
    simpleResponse "レベルを上げる" talkLevelUpDone
      & successEffect .~
        [ setStringVar valSpellLevelUp (T.pack $ "spell_" <> showVal valBaseMonster <> "_level_" <> showVal valSelectLevel) True
        , npcCastSpell valSpellLevelUp False
        , EffectMath $ valCurrentExp -= valConsumeExp
        , EffectMath $ valLevel =: valSelectLevel
        , EffectMath $ valNextLevel =: valLevel + (1 :: Int)
        , runEocs (initStatus ^. id)
        ]

talkShowStatus :: Talk
talkShowStatus =
  let statusText = T.pack $ unlines
                    [ "レベル: " <> showVal valTmpLevel
                    , "HP: "
                      <> showVal valTmpHp
                      <> "/"
                      <> showVal valTmpMaxHp
                    , "速度: " <> showVal valTmpSpeed
                       <> " 回避: " <> showVal valTmpDodge
                    , "耐打: " <> showVal valTmpBash
                       <> " 耐弾: " <> showVal valTmpBullet
                       <> " 耐斬: " <> showVal valTmpCut
                       <> " 耐刺: " <> showVal valTmpStab
                    , "戦闘スキル: " <> showVal valTmpMeleeSkill
                    , "経験値: " <> showVal valTmpCurrentExp
                    ]
   in returnTalk idShowStatus $ def
    & dynamicLine .~ DynamicLineText statusText
    & responses .~ [ responseBack ]

friendTalk :: Talk
friendTalk = talkMain
