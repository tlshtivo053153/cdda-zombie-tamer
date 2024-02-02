{-# LANGUAGE OverloadedStrings #-}
module Cdda.Talk.Utils
  ( mergeId
  , returnTalk
  , simpleResponse
  , responseDone
  , responseBack
  , responseTop
  ) where

import Prelude hiding (id)
import Define.Core
import Define.Talk
import Define.MakeFields

import Data.Text (Text)
import Data.Default

import Control.Lens

mergeId :: Id -> Id -> Id
mergeId (Id monId) (Id tId) = Id $ "TALK_" <> monId <> "_" <> tId

returnTalk :: Id -> Talk -> TalkAction Talk
returnTalk tId t = do
  monId <- view monsterId
  return $ t
    & talkId .~ mergeId monId tId

simpleResponse :: Text -> Talk -> Response
simpleResponse rtext rtalk = def
  & text .~ rtext
  & success .~ rtalk

responseDone :: Response
responseDone = def
  & text .~ "会話を終了"
  & success .~ TalkDone

responseBack :: Response
responseBack = def
  & text .~ "戻る"
  & success .~ TalkBack

responseTop :: Response
responseTop = def
  & text .~ "メインメニューに戻る"
  & success .~ TalkTop
