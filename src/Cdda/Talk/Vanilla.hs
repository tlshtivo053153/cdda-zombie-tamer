{-# LANGUAGE OverloadedStrings #-}
module Cdda.Talk.Vanilla
  ( vanillaTalk ) where

import Prelude hiding (id)

import Define.Core
import Define.Monster
import Define.Talk
import Define.MakeFields

import Cdda.Talk.Utils
import Cdda.Talk.Config

import Cdda.Id.Spell

import Control.Monad.Reader

import Control.Lens

vanillaTalk :: Monster -> [Talk]
vanillaTalk mon =
  runReader
    (sequence [ talkMain, talkFriendTamed ])
    $ getVanillaTalkConfig mon

idMain :: Id
idMain = Id "MAIN"

idFriendTamed :: Id
idFriendTamed = Id "FRIEND_TAMED"

_effectsToFriend :: Id -> Int -> Reader TalkConfig [Effect]
_effectsToFriend consumeItem n = do
  (Id monId) <- view monsterId
  let spellId = Id $ "spell_" <> monId <> "_to_friend"
  return
    [ NpcCastSpell spellId False
    , UConsumeItem consumeItem n
    ]

effectsToFriend :: Reader TalkConfig [Effect]
effectsToFriend = do
  monId <- view monsterId
  let spellId = idSpellToFriend monId
  return [ NpcCastSpell spellId False ]

talkMain :: Reader TalkConfig Talk
talkMain = do
  friendTamed <- simpleTrial <$> join (makeTResponse <$> effectsToFriend <*> talkFriendTamed)
  makeTalk idMain
           Nothing
           (DynamicLineText "(ゾンビに話しかけたときのフレーバー)")
           [ makeResponse "友達になる" friendTamed ConditionNone
           , makeResponse "会話を終了" trialTalkDone ConditionNone
           ]

talkFriendTamed :: Reader TalkConfig Talk
talkFriendTamed = do
  makeTalkSimple idFriendTamed
    "あなたと友達になりました"
    "・・・"
    trialTalkDone
    []
