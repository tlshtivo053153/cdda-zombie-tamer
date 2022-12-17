module Cdda.MonsterGroup where

import Define.Core
import Define.Monster
import Define.MonsterGroup
import Define.Talk

import Cdda.Id.Monster
import Cdda.Id.MonsterGroup
import Cdda.Id.Friend

mgNormal :: MonsterGroup
mgFat :: MonsterGroup
mgMedical :: MonsterGroup
mgBoomer :: MonsterGroup
mgRust :: MonsterGroup
mgLabsecurity :: MonsterGroup
mgElectric :: MonsterGroup
mgSkeleton :: MonsterGroup

toFriendMonster :: [(Id, Int)] -> [MGMonster]
toFriendMonster = map (\(m, w) -> MGMonster (monFriend m 1) w)

mgNormal = MonsterGroup idNormal $ toFriendMonster
  [ (,) monZombieGrabber 75
  , (,) monZombieRunner 75
  , (,) monZombieSmoker 75
  , (,) monZombieShady 75
  , (,) monZombieShrieker 75
  , (,) monZombieAcidic 75
  , (,) monZombieNecro 10
  , (,) monZombieBrute 75
  , (,) monZombieMaster 10
  , (,) monZombieHollow 75
  , (,) monZombieThorny 75
  , (,) monZombieStatic 75
  , (,) monZombieWinged 75
  , (,) monZombieRegenerating 75
  , (,) monZombieBruteGrappler 75
  ]

mgFat = MonsterGroup idFat $ toFriendMonster
  [ (,) monBoomer 50
  , (,) monZombiePupa 25
  , (,) monZombiePupa 25
  ]

mgMedical = MonsterGroup idMedical $ toFriendMonster
  [ (,) monZombieMedicalBrute 1
  , (,) monZombieMedicalRegenerating 1
  , (,) monZombieMedicalAcidic 1
  , (,) monSkeletonMedical 1
  , (,) monZombieHollow 1
  ]

mgBoomer = MonsterGroup idBoomer $ toFriendMonster
  [ (,) monBoomerHuge 1
  , (,) monBoomerGlutton 1
  , (,) monZombieNecroBoomer 1
  ]

mgRust = MonsterGroup idRust $ toFriendMonster
  [ (,) monZombieShell 1
  , (,) monZombiePlated 1
  , (,) monZombieUrchin 1
  , (,) monZombieHammerHands 1
  ]

mgLabsecurity = MonsterGroup idLabsecurity $ toFriendMonster
  [ (,) monZombieGrabber 1
  , (,) monZombieRunner 1
  , (,) monZombieBrute 1
  , (,) monZombieHollow 1
  , (,) monZombieRegenerating 1
  , (,) monZombieBruteGrappler 1
  ]

mgElectric = MonsterGroup idElectric $ toFriendMonster
  [ (,) monZombieBruteShocker 1
  , (,) monSkeletonElectric 1
  , (,) monZombieNullfield 1
  ]

mgSkeleton = MonsterGroup idSkeleton $ toFriendMonster
  [ (,) monSkeletonBrute 1
  , (,) monSkeletonNecro 1
  , (,) monSkeletonMaster 1
  ]

upgradeRandomTypeToId :: UpgradeRandomType -> Maybe Id
upgradeRandomTypeToId urt = case urt of
                                URNormal      -> Just idNormal
                                URFat         -> Just idFat
                                URMedical     -> Just idMedical
                                URBoomer      -> Just idBoomer
                                URRust        -> Just idRust
                                URLabsecurity -> Just idLabsecurity
                                URElectric    -> Just idElectric
                                URSkeleton    -> Just idSkeleton
                                URNone        -> Nothing

