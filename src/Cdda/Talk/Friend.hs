{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Cdda.Talk.Friend where

import Prelude hiding (id)

import Define.Core
import Define.Monster
import Define.Talk
import Define.MakeFields

import Cdda.Talk.Utils
import Cdda.Talk.Config

import qualified Cdda.Monster as M
import qualified Cdda.Item as I
import Cdda.Id.Item
import Cdda.Id.Spell
import Cdda.Monster.Exp
import Cdda.Item

import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe
import Control.Monad.Reader

import Control.Lens

opLT :: Op
opLTEQ :: Op
opGT :: Op
opGTEQ :: Op
opEQ :: Op
opNotEQ :: Op

opLT = Op "<"
opLTEQ = Op  "<="
opGT = Op ">"
opGTEQ = Op ">"
opEQ = Op "=="
opNotEQ = Op "!="

varCurrentExp :: Var
varTotalExp :: Var
varFriendship :: Var

varCurrentExp = Var "zombie_current_exp" "counter" "friend"
varTotalExp   = Var "zombie_total_exp" "counter" "friend"
varFriendship = Var "zombie_friendship" "counter" "friend"

valVarTmpCurrentExp :: Val
valVarTmpTotalExp :: Val
valVarTmpNeedExp :: Val
valVarTmpFriendship :: Val

valVarTmpCurrentExp = UValVar "tmp_zombie_current_exp"
valVarTmpTotalExp = UValVar "tmp_zombie_total_exp"
valVarTmpNeedExp = UValVar "tmp_zombie_need_exp"
valVarTmpFriendship = UValVar "tmp_zombie_friendship"

toUValVar :: Var -> Val
toNpcValVar :: Var -> Val

toUValVar (Var n t c) = UValVar $ t <> "_" <> c <> "_" <> n
toNpcValVar (Var n t c) = NpcValVar $ t <> "_" <> c <> "_" <> n

showVal :: Val -> String
showVal (UValVar val) = "<u_val:" <> T.unpack val <> ">"
showVal (NpcValVar val) = "<npc_val:" <> T.unpack val <> ">"

nextLevelIndex :: Reader TalkConfig Int
nextLevelIndex = view level

nextLevelExp :: Reader TalkConfig (Maybe Int)
nextLevelExp = do
  needExp' <- view needExp
  i <- nextLevelIndex
  return $ needExp' V.!? i

responseFeed :: Reader TalkConfig Response
responseFeed = simpleResponse "餌を与える" =<< talkFeed

responseUpgradeRandom :: Reader TalkConfig (Maybe Response)
responseUpgradeRandom = do
  (UpgradeRandom uc urt) <- view upgradeRandom
  toTrial uc urt
  where
    toTrial uc urt = do
      let cond = case (uc, urt) of
                   (UCFalse, _) -> Nothing
                   (_, URNone) -> Nothing
                   _ -> Just ConditionNone
      t <- fmap simpleTrial $ makeTResponse [] =<< talkUpgradeRandom
      return $ makeResponse "ランダム進化" t <$> cond

responseUpgradeStandard :: Reader TalkConfig (Maybe Response)
responseUpgradeStandard = do
  uss <- view upgradeStandard
  if null uss
     then return Nothing
     else fmap Just $ simpleResponse "通常進化" =<< talkUpgradeStandard

responseLevelUp :: Reader TalkConfig (Maybe Response)
responseLevelUp = do
  t <- fmap simpleTrial $ makeTResponse [] =<< talkLevelUp
  e <- nextLevelExp
  return $ case e of
            Nothing -> Nothing
            Just e' -> Just $ makeResponse "レベルアップ可能" t
                            $ ConditionCompareVar
                            $ NpcCompareVar varCurrentExp opGTEQ e'

responseSpecialAction :: Reader TalkConfig (Maybe Response)
responseSpecialAction = return Nothing

responseShowStatus :: Reader TalkConfig Response
responseShowStatus = do
  t <- fmap simpleTrial $ makeTResponse [ EffectArithmetic
                                           $ ArithmeticAssign valVarTmpCurrentExp
                                             $ ArithmeticVal (toNpcValVar varCurrentExp)
                                        , EffectArithmetic
                                           $ ArithmeticAssign valVarTmpTotalExp
                                             $ ArithmeticVal (toNpcValVar varTotalExp)
                                        , EffectArithmetic
                                           $ ArithmeticAssign valVarTmpFriendship
                                             $ ArithmeticVal (toNpcValVar varFriendship)
                                        ] =<< talkShowStatus
  return $ makeResponse "ステータス" t ConditionNone

talkMain :: Reader TalkConfig Talk
talkMain = do
  makeTalk (Id "MAIN")
           Nothing
           (DynamicLineText "友達ゾンビに話しかけたときのフーレバー")
           . catMaybes =<< sequence
                [ Just <$> responseFeed
                , responseUpgradeRandom
                , responseUpgradeStandard
                , responseLevelUp
                , responseSpecialAction
                , Just <$> responseShowStatus
                , return $ Just $ makeResponse "会話を終える" trialTalkDone ConditionNone
                ]

talkItemFed :: Reader TalkConfig Talk
talkItemFed = do
  t <- fmap simpleTrial $ makeTResponse [] =<< talkFeed
  makeTalk (Id "ITEM_FED")
           Nothing
           (DynamicLineText "よろこんでいるように見えます")
           [ makeResponse "・・・" t ConditionNone ]

talkFeedItem :: Id -> Reader TalkConfig Talk
talkFeedItem itemId@(Id itemText) = do
  let backResponse = [ simpleResponse "メインメニュー" =<< talkMain
                     , simpleResponse "戻る" =<< talkFeed
                     ]
      chooseNumResponse = flip map [1,2,4,8,16,32,64,128] $ \x -> do
        t <- fmap simpleTrial $ makeTResponse [ UConsumeItem itemId x
                                              , NpcAdjustVar varCurrentExp
                                                             $ x * itemExp itemId
                                              , NpcAdjustVar varTotalExp
                                                             $ x * itemExp itemId
                                              ] =<< talkItemFed
        return $ makeResponse (T.pack (show x) <> "個") t
               $ UHasItems itemId x
  makeTalk (Id $ "FEED_" <> T.toUpper itemText)
           Nothing
           (DynamicLineText "個数を選択")
           =<< sequence (chooseNumResponse ++ backResponse)

responseFeedItem :: Reader TalkConfig [Response]
responseFeedItem = mapM (\(i, t) -> simpleResponse t =<< talkFeedItem i)
  $ mapMaybe (\i -> (i,) <$> idToName i)
    [ idTaintedMeatPremium
    , idTaintedMeatHighPremium
    , idTaintedMarrowPremium
    , idTaintedMarrowHighPremium
    ]

talkFeed :: Reader TalkConfig Talk
talkFeed = do
  rs <- responseFeedItem
  resMain <- simpleResponse "戻る" =<< talkMain
  makeTalk (Id "FEED")
           Nothing
           (DynamicLineText "餌の種類を選択")
           $ rs ++ [ resMain ]

talkUpgradeDone :: Reader TalkConfig Talk
talkUpgradeDone = do
  makeTalk (Id "UPGRADE_DONE")
            Nothing
            (DynamicLineText "全身が痙攣を起こして喜んでいるように見えます")
            [ makeResponse "・・・" trialTalkDone ConditionNone ]

talkUpgradeRandomMonster :: Reader TalkConfig Talk
talkUpgradeRandomMonster = do
  (UpgradeRandom uc urt) <- view upgradeRandom
  let consume = case uc of
                 UCTrue -> Nothing
                 UCFalse -> Nothing
                 UCHaveItem itemId n -> Just $ UConsumeItem itemId n
  let cond = case uc of
               UCHaveItem itemId n -> UHasItems itemId n
               _ -> ConditionNone
  t <- fmap simpleTrial $ makeTResponse
                              (catMaybes
                                [ flip NpcCastSpell False <$> idSpellUpgradeRandom urt
                                , consume
                                , Just $ EffectArithmetic $
                                    ArithmeticAssign (toNpcValVar varCurrentExp)
                                    $ ArithmeticVal (toNpcValVar varTotalExp)
                                ]
                              ) =<< talkUpgradeDone
  backResponse <- simpleResponse "戻る" =<< talkUpgradeRandom
  makeTalk (Id "UPGRADE_RANDOM_MONSTER")
            Nothing
            (DynamicLineText "(口を開けている)")
            [ makeResponse "口の中に餌を入れる" t cond
            , backResponse
            ]

talkNotUpgrade :: Reader TalkConfig Talk
talkNotUpgrade = do
--  t <- fmap simpleTrial $ makeTResponse [] =<< talkUpgradeRandom
  let t = simpleTrial $ TResponse (Id "TALK_NONE") []
  makeTalk (Id "NOT_UPGRADE")
            Nothing
            (DynamicLineText "与えるアイテムがない")
            [ makeResponse "戻る" t ConditionNone ]

talkUpgradeRandom :: Reader TalkConfig Talk
talkUpgradeRandom = do
  (UpgradeRandom uc _) <- view upgradeRandom
  upgradeTresponse <- makeTResponse [] =<< talkUpgradeRandomMonster
  notUpgradeTresponse <- makeTResponse [] =<< talkNotUpgrade
  let ucText = case uc of
                 UCTrue -> Just "要求アイテムなし"
                 UCFalse -> Nothing
                 UCHaveItem itemId n -> do
                   itemName <- idToName itemId
                   return $ "[" <> itemName <> T.pack (show n) <> "]" <> " 餌を与える"
      c = case uc of
            UCTrue -> Just ConditionNone
            UCFalse -> Nothing
            UCHaveItem i n -> Just $ UHasItems i n
      t = fmap (\x -> makeTrial x upgradeTresponse notUpgradeTresponse) c
  let randomUpgradeResponse = maybeToList $ makeResponse <$> ucText <*> t <*> Just ConditionNone
  backResponse <- simpleResponse "戻る" =<< talkMain
  makeTalk (Id "UPGRADE_RANDOM")
            Nothing
            (DynamicLineText "ランダム進化メニュー")
            $ randomUpgradeResponse ++ [backResponse]

talkUpgradeStandardMonster :: UpgradeStandard -> Reader TalkConfig Talk
talkUpgradeStandardMonster us@(UpgradeStandard uc (Id monId)) = do
  let consume = case uc of
              UCHaveItem i n -> Just $ UConsumeItem i n
              _ -> Nothing
--  tr1 <- makeTResponse (catMaybes
  t <- fmap simpleTrial <$> makeTResponse (catMaybes
                          [ Just $ NpcCastSpell (idSpellUpgradeStandard us) False
                          , consume
                          , Just $ EffectArithmetic $
                              ArithmeticAssign (toNpcValVar varCurrentExp)
                              $ ArithmeticVal (toNpcValVar varTotalExp)
                          ]
                       ) =<< talkUpgradeDone
-- tr2 <- makeTResponse [] =<< talkNotUpgrade
--  let t = makeTrial ConditionNone tr1 tr2
  let cond = case uc of
               UCHaveItem itemId n -> UHasItems itemId n
               _ -> ConditionNone
  makeTalk (Id $ "UPGRADE_STANDARD_MONSTER_" <> T.toUpper monId)
            Nothing
            (DynamicLineText "(口を開けている)")
            =<< sequence
              [ return $ makeResponse "口の中に餌を入れる" t cond
              , simpleResponse "戻る" =<< talkUpgradeStandard
              ]
upgradeStandardToResponse :: UpgradeStandard -> Reader TalkConfig Response
upgradeStandardToResponse us = do
--  let cond = case uc of
--              UCTrue -> ConditionNone
--              UCFalse -> ConditionNone
--              UCHaveItem itemId n -> UHasItems itemId n
--      trTrue = makeTResponse []
--      t = makeTrial cond undefined undefined
  t <- fmap simpleTrial $ makeTResponse  [] =<< talkUpgradeStandardMonster us
  -- todo: text to "[必要アイテム n個] Xに進化"
  return $ makeResponse (usToText us) t ConditionNone
    where
      usToText (UpgradeStandard uc monId) =
        case uc of
          UCTrue -> fromMaybe "" (M.idToName monId)
          UCFalse -> "no upgrade"
          UCHaveItem itemId n -> "[" <> fromMaybe "" (I.idToName itemId) <> " " <> T.pack (show n) <> "] "
                                      <> fromMaybe "" (M.idToName monId)
talkUpgradeStandard :: Reader TalkConfig Talk
talkUpgradeStandard = do
  uss <- map upgradeStandardToResponse <$> view upgradeStandard
  backResponse <- simpleResponse "戻る" =<< talkMain
  makeTalk (Id "UPGRADE_STANDARD")
            Nothing
            (DynamicLineText "通常進化メニュー")
            =<< sequence
            (uss ++ [return backResponse])

talkLevelUpDone :: Reader TalkConfig Talk
talkLevelUpDone = do
  makeTalk (Id "LEVEL_UP_DONE")
            Nothing
            (DynamicLineText "レベルが上がりました")
            [ makeResponse "・・・" trialTalkDone ConditionNone ]

talkLevelUp :: Reader TalkConfig Talk
talkLevelUp = do
  l <- view level
  exps <- view needExp
  monId <- view monsterBase
  let levelList = mapMaybe (\x -> (x,) <$> exps V.!? (l + x - 1)) [1,2,4,8,16,32,64,99]
  let chooseLevelResponse = flip map levelList $ \(x, y) -> do
        let nextLevel = l + x
        t <- fmap simpleTrial $ makeTResponse
                                  [ NpcCastSpell (idSpellLevelUp monId nextLevel) False
                                  , NpcAdjustVar varCurrentExp (-y)
                                  ] =<< talkLevelUpDone
        return $ makeResponse ("[レベル+" <> T.pack (show x) <> "] "
                                <> "レベル" <> T.pack (show nextLevel)
                              ) t
                              $ ConditionCompareVar
                                $ NpcCompareVar varCurrentExp opGTEQ y
  let backResponse = simpleResponse "戻る" =<< talkMain
  makeTalk (Id "LEVEL_UP")
            Nothing
            (DynamicLineText $ "レベルアップメニュー(現在レベル" <> T.pack (show l) <> ")")
            =<< sequence (chooseLevelResponse ++ [backResponse])

--talkSpecialAction = undefined

talkShowStatus :: Reader TalkConfig Talk
talkShowStatus = do
  s <- view status
  level' <- view level
  let showLens t l = t <> show (s^.l)
      statusText = T.pack $ unlines
                    [ "レベル: " <> show level'
                    , showLens "HP: "hp
                       <> showLens " 速度: " speed
                       <> showLens " 回避: " dodge
                    , showLens "耐打: " (armor.bash)
                       <> showLens " 耐弾: " (armor.bullet)
                       <> showLens " 耐斬: " (armor.cut)
                       <> showLens " 耐刺: " (armor.stab)
                    , showLens "戦闘スキル: " (melee.skill)
                    , "経験値: " <> showVal valVarTmpCurrentExp
                    ]
  t <- fmap simpleTrial $ makeTResponse [] =<< talkMain
  makeTalkSimple (Id "SHOW_STATUS")
                  statusText
                  "・・・"
                  t
                  []

friendTalk :: Monster -> [(Int, [Talk])]
friendTalk mon =
  let talkFeedItems = map talkFeedItem
        [ idTaintedMeatPremium
        , idTaintedMeatHighPremium
        , idTaintedMarrowPremium
        , idTaintedMarrowHighPremium
        ]
      talkUpgradeStandardMonster' = map talkUpgradeStandardMonster $ mon^.upgradeStandard
      talkList = sequence $ concat
        [ [ talkMain
          , talkItemFed
          ]
        , talkFeedItems
        , [ talkFeed
          , talkUpgradeDone
          , talkUpgradeRandomMonster
          , talkNotUpgrade
          , talkUpgradeRandom
          ]
        , talkUpgradeStandardMonster'
        , [ talkUpgradeStandard
          , talkLevelUpDone
          , talkLevelUp
          , talkShowStatus
          ]
        ]
  in getFriendTalkConfig mon
      & map (\tc -> (tc ^. level, runReader talkList tc))
