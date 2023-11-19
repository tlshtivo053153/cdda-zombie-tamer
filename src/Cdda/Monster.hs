module Cdda.Monster
  ( allMonsterFriend
  , getMonsterFriend
  ) where

import Data.Maybe
import Define.Core
import Define.Monster

import qualified Cdda.Id.Monster as I

import Cdda.Monster.Status
import Cdda.Monster.Growth
import Cdda.Monster.Strength
import Cdda.Monster.Petfood
import Cdda.Monster.Upgrade

allMonsterFriend :: [Monster]
allMonsterFriend = mapMaybe getMonsterFriend I.allMonster

getMonsterFriend :: Id -> Maybe Monster
getMonsterFriend i = do
  status <- getStatus i
  growth <- getGrowth i
  strength <- getStrength i
  cost <- getFriendCost i
  return $ Monster
    { _monsterBase            = i
    , _monsterStatus          = status
    , _monsterGrowth          = growth
    , _monsterStrength        = strength
    , _monsterPetfood         = getPetFood i
    , _monsterFriendCost      = cost
    , _monsterUpgradeRandom   = getUpgradeRandom i
    , _monsterUpgradeStandard = getUpgradeStandard i
    }

