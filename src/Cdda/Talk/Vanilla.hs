{-# LANGUAGE OverloadedStrings #-}
module Cdda.Talk.Vanilla
  ( vanillaTalk ) where

import Prelude hiding (id)

import Define.Core
import Define.Talk
import Define.EOC
import Define.MakeFields

import Cdda.Talk.Utils
import Cdda.EOC
import Cdda.EOC.Math

import Data.Default
import qualified Data.Text as T

import Control.Lens

vanillaTalk :: Talk
vanillaTalk = talkMain

showVal :: Val -> String
showVal (UVal val) = "<u_val:" <> T.unpack val <> ">"
showVal (NpcVal val) = "<npc_val:" <> T.unpack val <> ">"
showVal (ContextVal val) = "<context_val:" <> T.unpack val <> ">"
showVal (GlobalVal val) = "<global_val:" <> T.unpack val <> ">"
showVal (VarVal _) = ""

idMain :: Id
idMain = Id "VANILLA_MAIN"

idFriendTamed :: Id
idFriendTamed = Id "VANILLA_FRIEND_TAMED"

valSpellToFriend :: Val
valSpellToFriend = ContextVal "spell_to_friend"

valIsInitialize :: Val
valIsInitialize = NpcVal "is_initialize"

effectsToFriend :: [Effect]
effectsToFriend =
  [ EffectMath $ valLevel =: (1 :: Int)
  , EffectMath $ valNextLevel =: (2 :: Int)
  , runEocs (initStatus ^. id)
  , NpcAddVar valIsInitialize "yes"
  , setStringVar valSpellToFriend (T.pack $ "spell_" <> showVal valBaseMonster <> "_to_friend") True
  , npcCastSpell valSpellToFriend False
  ]

talkMain :: Talk
talkMain =
  returnTalk idMain $ def
    & dynamicLine .~ DynamicLineText "(ゾンビに話しかけたときのフレーバー)"
    & responses .~
      [ simpleResponse "友達になる" talkFriendTamed
          & successEffect .~ effectsToFriend
      , responseDone
      ]

talkFriendTamed :: Talk
talkFriendTamed = returnTalk idFriendTamed $ def
  & dynamicLine .~ DynamicLineText "あなたと友達になりました。"
  & responses .~ [ responseDone ]
