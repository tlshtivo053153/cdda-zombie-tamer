module Cdda.Harvest where

import Define.Harvest
import Define.Monster

import Cdda.Id.Harvest
import Cdda.Id.Item

import Cdda.Monster.Strength

harvestZombie :: Strength -> Harvest
harvestZombie strength@(Strength s) = Harvest (idHarvestZombie strength)
  [ EntryRatio idTaintedMeat Flesh 0.25
  , EntryRatio idTaintedBlood Blood 0.1
  , EntryRatio idTaintedFat Flesh 0.08
  , EntryRatio idTaintedMarrow Bone 0.005
  , EntryRatio idTaintedBone Bone 0.1
  , EntryBase idTaintedMeatPremium Flesh (1, 3) (0, 0.12*s')
  , EntryBase idTaintedMeatHighPremium Flesh (1, 3) (0, 0.0012*s')
  , EntryBase idTaintedMarrowPremium Bone (1, 3) (0, 003*s')
  , EntryBase idTaintedMarrowHighPremium Bone (1, 3) (0, 00003*s')
  ]
    where
      s' = 1.12^s + 1*fromIntegral s

harvestSkeleton :: Strength -> Harvest
harvestSkeleton strength@(Strength s) = Harvest (idHarvestSkeleton strength)
  [ EntryRatio idTaintedBone Flesh 0.5
  , EntryRatio idTaintedMarrow Bone 0.005
  , EntryRatio idTaintedBlood Blood 0.01
  , EntryRatio idSinew Bone 0.001
  , EntryBase idTaintedMarrowPremium Bone (1, 3) (0, 0.12*s')
  , EntryBase idTaintedMarrowHighPremium Bone (1, 3) (0, 0.0012*s')
  ]
    where
      s' = 1.12^s + 1*fromIntegral s

allHarvest :: [Harvest]
allHarvest = zombie ++ skeleton
  where
    zombie = map harvestZombie allZombieStrength
    skeleton = map harvestSkeleton allSkeletonStrength
