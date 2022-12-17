{-# LANGUAGE OverloadedStrings #-}
module Cdda.Talk.Vanilla
  ( vanillaTalk ) where

import Prelude hiding (id)
import Data.Text (Text)
import qualified Data.Text as T

import Define.Core
import Define.Monster
import Define.Talk
import Define.MakeFields

import Cdda.Talk.Utils
import Cdda.Talk.Config

import Cdda.Id.Item
import Cdda.Id.Spell

import Control.Monad.Reader

import Control.Lens

vanillaTalk :: Monster -> [Talk]
vanillaTalk mon =
  runReader
--    (sequence [ talkMain, talkFeedFriend, talkFriendTamed, talkNotHasItem ])
    (sequence [ talkMain, talkFriendTamed ])
--    $ TalkConfig (mon^.base) undefined
--    $ TalkConfig (mon^.base) undefined
    $ getVanillaTalkConfig mon

--talkZombie = undefined
--talkZombieCrawler = undefined
--talkZombieFat = undefined
--talkZombieRot = undefined
--talkZombieTough = undefined
--talkZombieResortDancer = undefined
--talkZombieResortBouncer = undefined
--talkZombieResortStaff = undefined
--talkZombieMedical = undefined
--talkZombieWinged = undefined
--talkZombieChild = undefined
--talkZombieScientist = undefined
--talkZombieLabsecurity = undefined
--talkZombieRunner = undefined
--talkZombieSwimmerBase = undefined
--talkZombieTechnicial = undefined
--talkZombieMiner = undefined
--talkZombiePrisoner = undefined
--talkZombiePrisonerFat = undefined
--talkZombiePrisonerTough = undefined

--talkZombie = undefined
--
--talkZombieCrawler = undefined
--
--talkZombieCop = undefined
--
--talkZombieFat = undefined
--
--talkZombieRot = undefined
--
--talkZombieMedical = undefined
--
--talkZombieWretched = undefined
--
--talkBoomer = undefined
--
--talkZombieRust = undefined
--
--talkZombieLabsecurity = undefined
--
--talkZombieSwimmerBase = undefined
--
--talkZombieTechnician = undefined
--
--talkZombieMiner = undefined
--
--talkZombieThorny = undefined
--
--talkSkeleton = undefined
--
--talkZombieStatic = undefined
--
--talkZombieHollow = undefined

idMain :: Id
idMain = Id "MAIN"

idFeedFriend :: Id
idFeedFriend = Id "FEED_FRIEND"

idFriendTamed :: Id
idFriendTamed = Id "FRIEND_TAMED"

idNotHasItem :: Id
idNotHasItem = Id "NOT_HAS_ITEM"

--trialFeedFriend :: Reader TalkConfig Trial
--trialFeedFriend = do
----  noTrial <$> join (liftM2 makeTResponse talkFeedFriend $ return [])
--  noTrial <$> (makeTResponse [] =<< talkFeedFriend)

trialTalkMain :: Reader TalkConfig Trial
trialTalkMain = do
  simpleTrial <$> (makeTResponse [] =<< talkMain)

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
--  (Id monId) <- view monsterId
  monId <- view monsterId
--  let spellId = Id $ "spell_" <> monId <> "_to_friend"
  let spellId = idSpellToFriend monId
  return [ NpcCastSpell spellId False ]

trialFromTalk talk =
  simpleTrial <$> (makeTResponse [] =<< talk)

talkMain :: Reader TalkConfig Talk
talkMain = do
--  talkTrial <- trialFeedFriend
--  friendTamed <- trialFromTalk talkFriendTamed
--  friendTamed <- noTrial <$> (makeTResponse ef =<< talkFriendTamed)
--  friendTamed <- noTrial <$> join (makeTResponse <$> effectsToFriend <*> talkFeedFriend)
  friendTamed <- simpleTrial <$> join (makeTResponse <$> effectsToFriend <*> talkFriendTamed)
--  makeTalkSimple (Id "MAIN")
--                 "(ゾンビに話しかけたときのフレーバー)"
--                 "友達になる"
----                 talkTrial
--                 friendTamed
--                 [ responseDone ]
  makeTalk (Id "MAIN")
           Nothing
           (DynamicLineText "(ゾンビに話しかけたときのフレーバー)")
           [ makeResponse "友達になる" friendTamed ConditionNone
           , makeResponse "会話を終了" trialTalkDone ConditionNone
           ]

--talkFeedFriend :: Reader TalkConfig Talk
--talkFeedFriend = do
--  (condition, resText, effectsToFriend') <- do
--    grade' <- view grade
--    let toFriendText itemText n = "[" <> itemText <> T.pack (show n) <> "つ] 友達になる"
--        f itemId n itemText = do
--          e <- _effectsToFriend itemId n
--          return (UHasItems itemId n, toFriendText itemText n, e)
--    case grade' of
--            Grade GradeMeat1 n   -> f idTaintedMeatPremium n "上級汚染肉"
--            Grade GradeMeat2 n   -> f idTaintedMeatHighPremium n "最上級汚染肉"
--            Grade GradeMarrow1 n -> f idTaintedMarrowPremium n "上級汚染骨髄"
--            Grade GradeMarrow2 n -> f idTaintedMarrowHighPremium n "最上級汚染骨髄"
--  resFriendTamed <- makeTResponse effectsToFriend' =<< talkFriendTamed
--  resNotHasItem <- makeTResponse [] =<< talkNotHasItem
--  trialTalkMain' <- trialTalkMain
--  makeTalk (Id "FEED_FRIEND")
--           Nothing
--           (DynamicLineText "友達メニューのフレーバー")
--           [ makeResponse resText
--              $ makeTrial condition resFriendTamed resNotHasItem
--           , makeResponse "戻る" trialTalkMain'
--           ]

talkFriendTamed :: Reader TalkConfig Talk
talkFriendTamed = do
  makeTalkSimple (Id "FRIEND_TAMED")
    "あなたと友達になりました"
    "・・・"
    trialTalkDone
    []

talkNotHasItem :: Reader TalkConfig Talk
talkNotHasItem = do
  trialTalkMain' <- trialTalkMain
  makeTalkSimple (Id "NOT_HAS_ITEM")
    "アイテムがありません"
    "戻る"
    trialTalkMain'
    []
