module Cdda.Harvest where

import Define.Harvest
import Define.Monster

import Cdda.Id.Harvest
import Cdda.Id.Item
import Cdda.Id.ItemGroup

import Cdda.Monster.Strength

harvestZombie :: Strength -> Harvest
harvestZombie strength@(Strength s) = Harvest (idHarvestZombie strength)
  [ EntryDrop (idItemGroupZombie s) TaintedFood
  , EntryDrop (idItemGroupZombie s) TaintedFood
  , EntryDrop (idItemGroupZombie s) TaintedFood
  ]
--  [ EntryRatio idTaintedMeat Flesh 0.25
--  , EntryRatio idTaintedBlood Blood 0.1
--  , EntryRatio idTaintedFat Flesh 0.08
--  , EntryRatio idTaintedMarrow Bone 0.005
--  , EntryRatio idTaintedBone Bone 0.1
--  , EntryRatio idTaintedMeatPremium Flesh meatP
--  , EntryRatio idTaintedMeatHighPremium Flesh meatHP
--  , EntryRatio idTaintedMarrowPremium Bone marrowP
--  , EntryRatio idTaintedMarrowHighPremium Bone marrowHP
--  ]
--    where
--      s' = fromIntegral s
--      meatP = 0.00058 * s'
--      meatHP = meatP / 100
--      marrowP = 0.0000166 * s'
--      marrowHP = marrowP / 100

harvestSkeleton :: Strength -> Harvest
harvestSkeleton strength@(Strength s) = Harvest (idHarvestSkeleton strength)
  [ EntryDrop (idItemGroupSkeleton s) TaintedFood
  , EntryDrop (idItemGroupSkeleton s) TaintedFood
  ]
--  [ EntryRatio idTaintedBone Flesh 0.5
--  , EntryRatio idTaintedMarrow Bone 0.005
--  , EntryRatio idTaintedBlood Blood 0.01
--  , EntryRatio idSinew Bone 0.001
--  , EntryRatio idTaintedMarrowPremium Bone $ 0.00058*s'
--  , EntryRatio idTaintedMarrowHighPremium Bone $ 0.0000058*s'
--  ]
--    where
--      s' = fromIntegral s

allHarvest :: [Harvest]
allHarvest = zombie ++ skeleton
  where
    zombie = map harvestZombie allZombieStrength
    skeleton = map harvestSkeleton allSkeletonStrength
