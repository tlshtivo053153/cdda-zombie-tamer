{-# LANGUAGE OverloadedStrings #-}
module Cdda.Talk.Vanilla
  ( vanillaTalk ) where

import Prelude hiding (id)

import Define.Core
import Define.Monster
import Define.Talk
import Define.EOC
import Define.MakeFields

import Cdda.Talk.Utils
import Cdda.Talk.Config
import Cdda.EOC

import Cdda.Id.Spell

import Data.Default

import Control.Monad.Reader

import Control.Lens

vanillaTalk :: Monster -> Talk
vanillaTalk mon = runReader talkMain $ getVanillaTalkConfig mon

idMain :: Id
idMain = Id "MAIN"

idFriendTamed :: Id
idFriendTamed = Id "FRIEND_TAMED"

_effectsToFriend :: Id -> Int -> Reader TalkConfig [Effect]
_effectsToFriend consumeItem n = do
  (Id monId) <- view monsterId
  let spellId = Id $ "spell_" <> monId <> "_to_friend"
  return
    [ npcCastSpell spellId False
    , uConsumeItem consumeItem n
    ]

effectsToFriend :: TalkAction [Effect]
effectsToFriend = do
  monId <- view monsterId
  let spellId = idSpellToFriend monId
  return [ npcCastSpell spellId False ]

talkMain :: TalkAction Talk
talkMain = do
  talkFriendTamed' <- talkFriendTamed
  effectsToFriend' <- effectsToFriend
  returnTalk idMain $ def
    & dynamicLine .~ DynamicLineText "(ゾンビに話しかけたときのフレーバー)"
    & responses .~
      [ simpleResponse "友達になる" talkFriendTamed'
          & successEffect .~ effectsToFriend'
      , responseDone
      ]

talkFriendTamed :: TalkAction Talk
talkFriendTamed = returnTalk idFriendTamed $ def
  & dynamicLine .~ DynamicLineText "あなたと友達になりました。"
  & responses .~ [ responseDone ]
