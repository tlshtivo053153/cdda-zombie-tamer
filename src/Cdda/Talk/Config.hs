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
import Cdda.Talk.Utils
import Cdda.Monster.Status
import Cdda.Monster.Petfood
import Cdda.Monster.Exp

import Data.List (zip4)
import qualified Data.Text as T
import qualified Data.Vector as V

import Control.Lens

--getVanillaTalkConfig :: Id -> Maybe TalkConfig
--getVanillaTalkConfig monId = do
--  status' <- getStatus monId
--  return $ TalkConfig
--    { _talkConfigMonsterId = monId
--    , _talkConfigUpgradeRandom = UpgradeRandom UCFalse URNone
--    , _talkConfigUpgradeStandard = []
--    , _talkConfigPetfood = getPetFood monId
--    , _talkConfigStatus = status'
--    , _talkConfigLevel = 0
--    , _talkConfigNeedExp = V.empty
--    }

getVanillaTalkConfig :: Monster -> TalkConfig
getVanillaTalkConfig mon = TalkConfig
  { _talkConfigMonsterId = mon^.base
  , _talkConfigUpgradeRandom = mon^.upgradeRandom
  , _talkConfigUpgradeStandard = mon^.upgradeStandard
  , _talkConfigPetfood = mon^.petfood
  , _talkConfigStatus = mon^.status
  , _talkConfigLevel = 0
  , _talkConfigNeedExp = V.empty
  }

getFriendTalkConfig :: Monster -> [TalkConfig]
getFriendTalkConfig mon =
--  let monId = mon^.base
--      mons = map (\l -> Id $ monId <> "_friend_lv" <> T.pack (show l)) lvs
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
        , _talkConfigUpgradeRandom = mon^.upgradeRandom
        , _talkConfigUpgradeStandard = mon^.upgradeStandard
        , _talkConfigPetfood = p
        , _talkConfigStatus = s
        , _talkConfigLevel = l
        , _talkConfigNeedExp = needExp'
        }
