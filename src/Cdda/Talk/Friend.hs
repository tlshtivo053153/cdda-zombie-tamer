{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Cdda.Talk.Friend
  ( friendTalk
  ) where

import Prelude hiding (id, (+), (-), (*), (>), (>=), (++))
import qualified Prelude as P

import Define.Core
import Define.Monster
import Define.Talk
import Define.MakeFields
import Define.EOC

import Cdda.Talk.Utils
import Cdda.Talk.Config

import qualified Cdda.Monster as M
import Cdda.Id.Item
import Cdda.Id.Spell
import Cdda.Monster.Exp
import Cdda.EOC.Math

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

idNotUpgrade :: Id
idNotUpgrade = Id "NOT_UPGRADE"

idUpgradeRandom :: Id
idUpgradeRandom = Id "UPGRADE_RANDOM"

idUpgradeStandardMonster :: UpgradeStandard -> Id
idUpgradeStandardMonster (UpgradeStandard _ (Id monId)) =
  Id $ "UPGRADE_STANDARD_MONSTER_" <> T.toUpper monId

idUpgradeStandard :: Id
idUpgradeStandard = Id "UPGRADE_STANDARD"

idLevelUpDone :: Id
idLevelUpDone = Id "LEVEL_UP_DONE"

idLevelUp :: Id
idLevelUp = Id "LEVEL_UP"

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
valTmpFoodId :: Val
valTmpFoodName :: Val
valTmpFoodHaveNum :: Val
valTmpFoodConsumeNum :: Val
valTmpFoodExp :: Val

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
valTmpFoodId = ContextVal "tmp_food_id"
valTmpFoodName = ContextVal "tmp_food_name"
valTmpFoodHaveNum = ContextVal "tmp_food_have_num"
valTmpFoodConsumeNum = ContextVal "tmp_food_consume_num"
valTmpFoodExp = ContextVal "tmp_food_exp"

showVal :: Val -> String
showVal (UVal val) = "<u_val:" <> T.unpack val <> ">"
showVal (NpcVal val) = "<npc_val:" <> T.unpack val <> ">"
showVal (ContextVal val) = "<context_val:" <> T.unpack val <> ">"
showVal (GlobalVal val) = "<global_val:" <> T.unpack val <> ">"
showVal (VarVal _) = ""

showItemName :: Id -> String
showItemName (Id itemName) = "<item_name:" <> T.unpack itemName <> ">"

nextLevelIndex :: TalkAction Int
nextLevelIndex = view level

nextLevelExp :: TalkAction (Maybe Int)
nextLevelExp = do
  needExp' <- view needExp
  i <- nextLevelIndex
  return $ needExp' V.!? i

responseMainFeed :: TalkAction Response
responseMainFeed = simpleResponse "餌を与える" <$> talkFeed

responseMainUpgradeRandom :: TalkAction (Maybe Response)
responseMainUpgradeRandom = do
  (UpgradeRandom uc urt) <- view upgradeRandom
  talkUpgradeRandomMain' <- talkUpgradeRandomMain
  return $ case (uc, urt) of
               (UCFalse, _) -> Nothing
               (_, URNone) -> Nothing
               _ -> Just $ simpleResponse "ランダム進化" talkUpgradeRandomMain'

responseMainUpgradeStandard :: TalkAction (Maybe Response)
responseMainUpgradeStandard = do
  uss <- view upgradeStandard
  if null uss
     then return Nothing
     else fmap Just $ simpleResponse "通常進化" <$> talkUpgradeStandard

responseMainLevelUp :: TalkAction (Maybe Response)
responseMainLevelUp = do
  talkLevelUp' <- talkLevelUp
  e <- nextLevelExp
  return $ case e of
            Nothing -> Nothing
            Just e' ->
              Just $ simpleResponse "レベルアップ可能" talkLevelUp'
                      & condition .~ ConditionMath (Math1 $ valCurrentExp >= e')

responseMainSpecialAction :: TalkAction (Maybe Response)
responseMainSpecialAction = return Nothing

responseMainShowStatus :: TalkAction Response
responseMainShowStatus = do
  talkShowStatus' <- talkShowStatus
  return $ simpleResponse "ステータス" talkShowStatus'
    & successEffect .~
      [ EffectMath $ valTmpCurrentExp =: valCurrentExp
      , EffectMath $ valTmpTotalExp =: valTotalExp
      , EffectMath $ valTmpHp =: MathExpr "n_hp('torso')"
      , EffectMath $ valTmpMaxHp =: MathExpr "n_hp_max('torso')"
      , EffectMath $ valTmpBash =: MathExpr "n_armor('bash', 'torso')"
      , EffectMath $ valTmpBullet =: MathExpr "n_armor('bullet', 'torso')"
      , EffectMath $ valTmpCut =: MathExpr "n_armor('cut', 'torso')"
      , EffectMath $ valTmpStab =: MathExpr "n_armor('stab', 'torso')"
      ]

talkMain :: TalkAction Talk
talkMain = do
  res <- catMaybes <$> sequence
    [ Just <$> responseMainFeed
    , responseMainUpgradeRandom
    , responseMainUpgradeStandard
    , responseMainLevelUp
    , Just <$> responseMainShowStatus
    , return $ Just responseDone
    ]
  returnTalk idMain $ def
    & dynamicLine .~ DynamicLineText "友達ゾンビに話しかけたときのフーレバー"
    & responses .~ res

talkItemFed :: TalkAction Talk
talkItemFed = do
  returnTalk idItemFed $ def
    & dynamicLine .~ DynamicLineText "よろこんでいるように見えます。"
    & responses .~ [ responseTop ]

talkFeedItem :: TalkAction Talk
talkFeedItem = do
  responseFeedItem' <- responseFeedItem
  let dlText = T.pack $ unlines [ "選択アイテム: " <> showVal valTmpFoodName
                                , "所持数: " <> showVal valTmpFoodHaveNum
                                , "増加経験値: " <> showVal valTmpFoodExp
                                ]
  returnTalk idFeedItem $ def
    & dynamicLine .~ DynamicLineText dlText
    & responses .~ responseFeedItem' <> [ responseBack ]

responseFeedItem :: TalkAction [Response]
responseFeedItem = do
  talkPreItemFed' <- talkPreItemFed
  return
    [ simpleResponse "1個使用" talkPreItemFed'
      & successEffect .~
        [ EffectMath $ valTmpFoodConsumeNum =: (1 :: Int) ]
    , simpleResponse "すべて使用" talkPreItemFed'
      & successEffect .~
        [ EffectMath $ valTmpFoodConsumeNum =: valTmpFoodHaveNum ]
    , simpleResponse "個数を選択" talkPreItemFed'
      & successEffect .~
        [ EffectMath $ valTmpFoodConsumeNum =: MathExpr "num_input('個数を入力', 0)"
        , EffectMath $ valTmpFoodConsumeNum =:
            mathFunc3 "clamp" valTmpFoodConsumeNum (0 :: Int) valTmpFoodHaveNum
        ]
    ]

talkNotFeedItem :: TalkAction Talk
talkNotFeedItem = do
  returnTalk idNotFeedItem $ def
    & dynamicLine .~ DynamicLineText "アイテムを持っていない"
    & responses .~ [ responseBack ]

responseFeed :: TalkAction [Response]
responseFeed = mapM responseFeed'
  [ idTaintedMeatPremium
  , idTaintedMeatHighPremium
  , idTaintedMarrowPremium
  , idTaintedMarrowHighPremium
  ]

responseFeed' :: Id -> TalkAction Response
responseFeed' itemId = do
  talkFeedItem' <- talkFeedItem
  talkNotFeedItem' <- talkNotFeedItem
  let itemText = T.pack $ showItemName itemId
  return $ simpleResponse itemText talkFeedItem'
    & trial .~ UHasItems itemId 1
    & successEffect .~
      [ SetStringVar valTmpFoodId (runId itemId) False
      , SetStringVar valTmpFoodName (T.pack $ showItemName itemId) True
      , EffectMath $ valTmpFoodExp =: itemExp itemId
      , EffectMath $ valTmpFoodHaveNum =: mathFunc1 "u_item_count"
                                                    (MathExpr $ "'" <> runId itemId <> "'")
      ]
    & failure ?~ talkNotFeedItem'

talkFeed :: TalkAction Talk
talkFeed = do
  rs <- responseFeed
  returnTalk idFeed $ def
    & dynamicLine .~ DynamicLineText "餌の種類を選択"
    & responses .~ rs <> [ responseBack ]

talkPreItemFed :: TalkAction Talk
talkPreItemFed = do
  rs <- responsePreItemFed
  let dlText = T.pack $ "使用個数: " <> showVal valTmpFoodConsumeNum
  returnTalk idPreItemFed $ def
    & dynamicLine .~ DynamicLineText dlText
    & responses .~ rs : [ responseBack ]

responsePreItemFed :: TalkAction Response
responsePreItemFed = do
  talkItemFed' <- talkItemFed
  return $ simpleResponse "餌を与える" talkItemFed'
    & condition .~ ConditionMath (Math1 $ valTmpFoodConsumeNum > (0 :: Int))
    & successEffect .~
      [ UConsumeItemVal valTmpFoodId valTmpFoodConsumeNum
      , EffectMath $ valCurrentExp += valTmpFoodConsumeNum * valTmpFoodExp
      , EffectMath $ valTotalExp += valTmpFoodConsumeNum * valTmpFoodExp
      ]

talkUpgradeDone :: TalkAction Talk
talkUpgradeDone = do
  returnTalk idUpgradeDone $ def
    & dynamicLine .~ DynamicLineText "全身が痙攣を起こして喜んでいるように見えます"
    & responses .~ [ responseDone ]

talkUpgradeRandomMonster :: TalkAction Talk
talkUpgradeRandomMonster = do
  res <- responseUpgradeRandomMonster
  returnTalk idUpgradeRandomMonster $ def
    & dynamicLine .~ DynamicLineText "(口を開けている)"
    & responses .~ [ res
                   , responseBack
                   ]

responseUpgradeRandomMonster :: TalkAction Response
responseUpgradeRandomMonster = do
  (UpgradeRandom uc urt) <- view upgradeRandom
  let consume = case uc of
                 UCTrue -> Nothing
                 UCFalse -> Nothing
                 UCHaveItem itemId n -> Just $ UConsumeItem itemId n
  let cond = case uc of
               UCHaveItem itemId n -> UHasItems itemId n
               _ -> ConditionNone
  talkUpgradeDone' <- talkUpgradeDone
  return $ simpleResponse "口の中に餌を入れる" talkUpgradeDone'
    & condition .~ cond
    & successEffect .~ catMaybes
      [ flip NpcCastSpell False <$> idSpellUpgradeRandom urt
      , consume
      , Just $ EffectMath $ valCurrentExp =: valTotalExp
      ]

talkNotRandomUpgrade :: TalkAction Talk
talkNotRandomUpgrade = do
  returnTalk idNotUpgrade $ def
    & dynamicLine .~ DynamicLineText "与えるアイテムがない"
    & responses .~ [ responseBack ]

talkUpgradeRandomMain :: TalkAction Talk
talkUpgradeRandomMain = do
  responseUpgradeRandomMain' <- responseUpgradeRandomMain
  returnTalk idUpgradeRandom $ def
    & dynamicLine .~ DynamicLineText "ランダム進化メニュー"
    & responses .~ responseUpgradeRandomMain' <> [responseBack]

responseUpgradeRandomMain :: TalkAction [Response]
responseUpgradeRandomMain = do
  (UpgradeRandom uc _) <- view upgradeRandom
  let ucText = case uc of
                 UCTrue -> Just "要求アイテムなし"
                 UCFalse -> Nothing
                 UCHaveItem itemId n ->
                   Just $ "[" <> T.pack (showItemName itemId) <> T.pack (show n) <> "]" <> " 餌を与える"
      c = case uc of
            UCTrue -> Just ConditionNone
            UCFalse -> Nothing
            UCHaveItem i n -> Just $ UHasItems i n
  talkUpgradeRandomMonster' <- talkUpgradeRandomMonster
  talkNotRandomUpgrade' <- talkNotRandomUpgrade
  return $ maybeToList $ do
    ucText' <- ucText
    c' <- c
    Just $ def
      & text .~ ucText'
      & trial .~ c'
      & success .~ talkUpgradeRandomMonster'
      & failure ?~ talkNotRandomUpgrade'

talkUpgradeStandardMonster :: UpgradeStandard -> TalkAction Talk
talkUpgradeStandardMonster us = do
  res <- responseUpgradeStandardMonster us
  returnTalk (idUpgradeStandardMonster us) $ def
    & dynamicLine .~ DynamicLineText "(口を開けている)"
    & responses .~ [ res
                   , responseBack
                   ]

responseUpgradeStandardMonster :: UpgradeStandard -> TalkAction Response
responseUpgradeStandardMonster us@(UpgradeStandard uc _) = do
  let consume = case uc of
              UCHaveItem i n -> Just $ UConsumeItem i n
              _ -> Nothing
      cond = case uc of
               UCHaveItem i n -> UHasItems i n
               _ -> ConditionNone
  talkUpgradeDone' <- talkUpgradeDone
  return $ simpleResponse "口の中に餌を入れる" talkUpgradeDone'
    & condition .~ cond
    & successEffect .~ catMaybes
      [ Just $ NpcCastSpell (idSpellUpgradeStandard us) False
      , consume
      , Just $ EffectMath $ valCurrentExp =: valTotalExp
      ]

talkUpgradeStandard :: TalkAction Talk
talkUpgradeStandard = do
  ress <- responseUpgradeStandard
  returnTalk idUpgradeStandard $ def
    & dynamicLine .~ DynamicLineText "通常進化メニュー"
    & responses .~ ress <> [ responseBack ]

responseUpgradeStandard :: TalkAction [Response]
responseUpgradeStandard = do
  mapM responseUpgradeStandard' =<< view upgradeStandard

responseUpgradeStandard' :: UpgradeStandard -> TalkAction Response
responseUpgradeStandard' us = do
  talkUpgradeStandardMonster' <- talkUpgradeStandardMonster us
  return $ simpleResponse (usToText us) talkUpgradeStandardMonster'
    where
      usToText (UpgradeStandard uc monId) =
        case uc of
          UCTrue -> fromMaybe "DID NOT FIND NAME" (M.idToName monId)
          UCFalse -> "no upgrade"
          UCHaveItem itemId n -> "[" <> T.pack (showItemName itemId) <> " " <> T.pack (show n) <> "] "

talkLevelUpDone :: TalkAction Talk
talkLevelUpDone = do
  returnTalk idLevelUpDone $ def
    & dynamicLine .~ DynamicLineText "レベルが上がりました"
    & responses .~ [ responseDone ]

talkLevelUp :: TalkAction Talk
talkLevelUp = do
  rss <- responseLevelUp
  l <- view level
  returnTalk idLevelUp $ def
    & dynamicLine .~ DynamicLineText ("レベルアップメニュー(現在レベル" <> T.pack (show l) <> ")")
    & responses .~ rss <> [ responseBack ]

responseLevelUp :: TalkAction [Response]
responseLevelUp = do
  l <- view level
  exps <- view needExp
  monId <- view monsterBase
  talkLevelUpDone' <- talkLevelUpDone
  let levelList = mapMaybe (\x -> (x,) <$> exps V.!? (l P.+ x P.- 1)) [1,2,4,8,16,32,64,99]
  return $ flip map levelList $ \(x, y) -> do
    let nextLevel = l P.+ x
        resText = "[レベル+" <> T.pack (show x) <> "] "
                    <> "レベル" <> T.pack (show nextLevel)
    simpleResponse resText talkLevelUpDone'
      & condition .~ ConditionMath (Math1 $ valCurrentExp >= y)
      & successEffect .~
        [ NpcCastSpell (idSpellLevelUp monId nextLevel) False
        , EffectMath $ valCurrentExp -= y
        ]

talkShowStatus :: TalkAction Talk
talkShowStatus = do
  s <- view status
  level' <- view level
  let showLens t l = t <> show (s^.l)
      statusText = T.pack $ unlines
                    [ "レベル: " <> show level'
                    , "HP: "
                      <> showVal valTmpHp
                      <> "/"
                      <> showVal valTmpMaxHp
                    , showLens "速度: " speed
                       <> showLens " 回避: " dodge
                    , "耐打: " <> showVal valTmpBash
                       <> " 耐弾: " <> showVal valTmpBullet
                       <> " 耐斬: " <> showVal valTmpCut
                       <> " 耐刺: " <> showVal valTmpStab
                    , showLens "戦闘スキル: " (melee.skill)
                    , "経験値: " <> showVal valTmpCurrentExp
                    ]
  returnTalk idShowStatus $ def
    & dynamicLine .~ DynamicLineText statusText
    & responses .~ [ responseBack ]

friendTalk :: Monster -> [(Int, Talk)]
friendTalk mon =
  getFriendTalkConfig mon
    & map (\tc -> (tc ^. level, runReader talkMain tc))
