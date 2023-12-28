{-# LANGUAGE OverloadedStrings #-}
module Cdda.Furniture
  ( meatSlime
  , marrowSlime
  , allFurniture
  ) where

import Define.Monster
import Define.Furniture

import Cdda.Id.Furniture
import Cdda.Id.ItemGroup
import Cdda.Monster.Strength

meatSlime :: Strength -> Furniture
meatSlime n = Furniture
  { _furnitureId          = idMeatSlime n
  , _furnitureName        = "肉スライム"
  , _furnitureDescription = "ゾンビの体から分離したスライムです。"
  , _furnitureItemGroup   = idItemGroupZombie n
  }

marrowSlime :: Strength -> Furniture
marrowSlime n = Furniture
  { _furnitureId          = idMarrowSlime n
  , _furnitureName        = "骨髄スライム"
  , _furnitureDescription = "ゾンビの体から分離したスライムです。"
  , _furnitureItemGroup   = idItemGroupSkeleton n
  }

allFurniture :: [Furniture]
allFurniture = z ++ s
  where
    z = map meatSlime allZombieStrength
    s = map marrowSlime allSkeletonStrength
