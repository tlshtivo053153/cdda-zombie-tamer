module Cdda.Harvest where

import Define.Harvest
import Define.Monster

import Cdda.Id.Harvest
import Cdda.Id.ItemGroup

import Cdda.Monster.Strength

harvestZombie :: Strength -> Harvest
harvestZombie strength@(Strength s) = Harvest (idHarvestZombie strength)
  [ EntryDrop (idItemGroupZombie s) TaintedFood
  , EntryDrop (idItemGroupZombie s) TaintedFood
  , EntryDrop (idItemGroupZombie s) TaintedFood
  ]

harvestSkeleton :: Strength -> Harvest
harvestSkeleton strength@(Strength s) = Harvest (idHarvestSkeleton strength)
  [ EntryDrop (idItemGroupSkeleton s) TaintedFood
  , EntryDrop (idItemGroupSkeleton s) TaintedFood
  ]

allHarvest :: [Harvest]
allHarvest = zombie ++ skeleton
  where
    zombie = map harvestZombie allZombieStrength
    skeleton = map harvestSkeleton allSkeletonStrength
