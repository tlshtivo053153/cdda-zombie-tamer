{-# LANGUAGE OverloadedStrings #-}
module Cdda.Talk.Utils
  ( mergeId
  , makeTalk
  , makeTalkSimple
  , makeResponse
  , simpleResponse
  , makeTResponse
  , makeTrial
  , simpleTrial
  , trialTalkDone
  , responseDone
  ) where

import Prelude hiding (id)
import Define.Core
import Define.Talk
import Define.MakeFields

import Data.Text (Text)

import Control.Monad.Reader

import Control.Lens

mergeId :: Id -> Id -> Id
mergeId (Id monId) (Id talkId) = Id $ "TALK_" <> monId <> "_" <> talkId

makeTalk :: Id -> Maybe Effect -> DynamicLine -> [Response] -> Reader TalkConfig Talk
makeTalk talkId eff dl rs = do
  monId <- view monsterId
  return $ Talk (mergeId monId talkId) eff dl rs

makeTalkSimple :: Id -> Text -> Text -> Trial -> [Response] -> Reader TalkConfig Talk
makeTalkSimple talkId dlText resText tr rs =
  makeTalk
    talkId
    Nothing
    (DynamicLineText dlText)
    $ makeResponse resText tr ConditionNone : rs

makeResponse :: Text -> Trial -> Condition -> Response
makeResponse = Response

simpleResponse :: Text -> Talk -> Reader TalkConfig Response
simpleResponse rtext rtalk = do
  t <- simpleTrial <$> makeTResponse [] rtalk
  return $ Response rtext t ConditionNone

makeTResponse :: [Effect] -> Talk -> Reader TalkConfig TResponse
makeTResponse es t = do
  let talkId = t ^. id
  return $ TResponse talkId es

makeTrial :: Condition -> TResponse -> TResponse -> Trial
makeTrial = Trial

--noTrial :: TResponse -> Trial
--noTrial tr = Trial ConditionNone tr emptyTResponse

simpleTrial :: TResponse -> Trial
simpleTrial tr = Trial ConditionNone tr emptyTResponse

trialTalkDone :: Trial
--trialTalkDone = noTrial $ TResponse (Id "TALK_DONE") []
trialTalkDone = simpleTrial $ TResponse (Id "TALK_DONE") []

responseDone :: Response
responseDone = makeResponse "会話を終了" trialTalkDone ConditionNone

emptyTResponse :: TResponse
emptyTResponse = TResponse (Id "TALK_EMPTY") []

