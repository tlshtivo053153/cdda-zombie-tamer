{-# LANGUAGE OverloadedStrings #-}
module Cdda.Furniture where

import Define.Monster
import Define.Furniture

import Cdda.Id.Furniture
import Cdda.Id.ItemGroup
import Cdda.Monster.Strength

meatSlime :: Strength -> Furniture
meatSlime s@(Strength n) = Furnitrue
  { _furnitureId          = idMeatSlime s
  , _furnitureName        = "肉スライム"
  , _furnitureDescription = "ゾンビの体から分離したスライムです。"
  , _furnitureItemGroup   = idItemGroupZombie n
  }

marrowSlime :: Strength -> Furniture
marrowSlime s@(Strength n) = Furnitrue
  { _furnitureId          = idMarrowSlime s
  , _furnitureName        = "骨髄スライム"
  , _furnitureDescription = "ゾンビの体から分離したスライムです。"
  , _furnitureItemGroup   = idItemGroupSkeleton n
  }

allFurniture :: [Furniture]
allFurniture = z ++ s
  where
    z = map meatSlime allZombieStrength
    s = map marrowSlime allSkeletonStrength
