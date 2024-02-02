module Cdda.Harvest
  ( harvestZombie
  , harvestSkeleton
  , allHarvest
  ) where

import Define.Harvest
import Define.Monster

import Cdda.Id.Harvest
import Cdda.Id.ItemGroup

import Cdda.Monster.Strength

harvestZombie :: Strength -> Harvest
harvestZombie s = Harvest (idHarvestZombie s)
  [ EntryDrop (idItemGroupZombie s) TaintedFood
  , EntryDrop (idItemGroupZombie s) TaintedFood
  , EntryDrop (idItemGroupZombie s) TaintedFood
  ]

harvestSkeleton :: Strength -> Harvest
harvestSkeleton s = Harvest (idHarvestSkeleton s)
  [ EntryDrop (idItemGroupSkeleton s) TaintedFood
  , EntryDrop (idItemGroupSkeleton s) TaintedFood
  ]

allHarvest :: [Harvest]
allHarvest = zombie ++ skeleton
  where
    zombie = map harvestZombie allZombieStrength
    skeleton = map harvestSkeleton allSkeletonStrength
