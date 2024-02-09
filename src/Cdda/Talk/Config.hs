{-# LANGUAGE OverloadedStrings #-}
module Cdda.Talk.Config
  ( getVanillaTalkConfig
  , getFriendTalkConfig ) where

import Prelude hiding (exp)
import Define.Core
import Define.Talk
import Define.Monster
import Define.MakeFields

import Cdda.Id.Friend
import Cdda.Monster.Status
import Cdda.Monster.Petfood
import Cdda.Monster.Exp
import Cdda.Talk.Utils

import Data.List (zip4)
import qualified Data.Vector as V

import Control.Lens

getVanillaTalkConfig :: Monster -> TalkConfig
getVanillaTalkConfig mon = TalkConfig
  { _talkConfigMonsterId = mon^.base
  , _talkConfigMonsterBase = mon^.base
  , _talkConfigPetfood = mon^.petfood
  , _talkConfigStatus = mon^.status
  , _talkConfigLevel = 0
  , _talkConfigNeedExp = V.empty
  , _talkConfigTopTalkId = mergeId (mon^.base) $ Id "MAIN"
  }

getFriendTalkConfig :: Monster -> [TalkConfig]
getFriendTalkConfig mon =
  let mons = map (monFriend $ mon^.base) lvs
      statuss = map (\l -> statusWithLevel l (mon^.growth) (mon^.status)) lvs
      petfoods = map costToPetfood $ V.toList needExp'
      lvs = [1..mon^.growth.maxLevel]
      needExp' = V.fromList $ map (calcExp mon) lvs
      xs :: [(Id, Status, PetFood, Int)]
      xs = zip4 mons statuss petfoods lvs
   in flip map xs $ \(m, s, p, l) ->
        TalkConfig
        { _talkConfigMonsterId = m
        , _talkConfigMonsterBase = mon^.base
        , _talkConfigPetfood = p
        , _talkConfigStatus = s
        , _talkConfigLevel = l
        , _talkConfigNeedExp = needExp'
        , _talkConfigTopTalkId = mergeId m $ Id "MAIN"
        }
