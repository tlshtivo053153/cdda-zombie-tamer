{-# LANGUAGE OverloadedStrings #-}
module Cdda.Monster
  ( allMonsterFriend
  , getMonsterFriend
  , idToName
  ) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Define.Core
import Define.Monster

import qualified Cdda.Id.Monster as I

import Cdda.Monster.Status
import Cdda.Monster.Growth
import Cdda.Monster.Strength
import Cdda.Monster.Petfood
import Cdda.Flag.Upgrade

allMonsterFriend :: [Monster]
allMonsterFriend = mapMaybe getMonsterFriend I.allFriendMonster

getMonsterFriend :: Id -> Maybe Monster
getMonsterFriend i = do
  status <- getStatus i
  growth <- getGrowth i
  strength <- getStrength i
  cost <- getFriendCost i
  petfood <- case getPetFood i of
    PetFood [] -> Nothing
    pf -> Just pf
  return $ Monster
    { _monsterBase            = i
    , _monsterStatus          = status
    , _monsterGrowth          = growth
    , _monsterStrength        = strength
    , _monsterPetfood         = petfood
    , _monsterFriendCost      = cost
    , _monsterFlags            = getRandom i ++ getStandard i
    }

idToName :: Id -> Maybe T.Text
idToName monId = M.lookup monId $ M.fromList
  [ (,) I.monGasZombie           "ガソリンゾンビ"
  , (,) I.monZombieKevlar1       "ケブラーゾンビ"
  , (,) I.monSkeleton            "骸骨ゾンビ"
  , (,) I.monZombieAcidic        "弱酸ゾンビ"
  , (,) I.monZombieRust          "鉄錆ゾンビ"
  , (,) I.monZombieThorny        "棘蔦ゾンビ"
  , (,) I.monZombieStatic        "感電ゾンビ"
  , (,) I.monSkeletonMedical     "医者ゾンビ"
  , (,) I.monZombieMedicalAcidic "弱酸医者ゾンビ"
  , (,) I.monZombieHunter        "ハンターゾンビ"
  , (,) I.monZombiePredator      "プレデターゾンビ"
  , (,) I.monDevourer            "融合ゾンビ"
  , (,) I.monZombieSpitter       "強酸ゾンビ"
  , (,) I.monZombieCorrosive     "腐食ゾンビ"
  , (,) I.monZombieSwimmer       "水泳ゾンビ"
  , (,) I.monZombieMancroc       "大顎ゾンビ"
  , (,) I.monZombieElectric      "感電ゾンビ"
  , (,) I.monShoggoth            "ショゴス"
  , (,) I.monZombieGrappler      "組手ゾンビ"
  , (,) I.monZombieBiter         "涎垂ゾンビ"
  , (,) I.monSmokerBrute         "凶暴煙幕ゾンビ"
  , (,) I.monZombieBruteNinja    "夜行ゾンビ"
  , (,) I.monZombieScreecher     "悲鳴ゾンビ"
  , (,) I.monSkeletonNecro       "骸骨ゾンビリッチ"
  , (,) I.monZombieHulk          "巨体ゾンビ"
  , (,) I.monSkeletonMaster      "骸骨ゾンビマスター"
  , (,) I.monZombieBruteWinged   "翼手凶暴ゾンビ"
  , (,) I.monSkeletonHulk        "巨体骸骨ゾンビ"
  ]
