module Cdda.TerFurnTransform
  ( allTerFurnTransform
  ) where

import Define.Monster
import Define.TerFurnTransform

import Cdda.Id.TerFurnTransform
import Cdda.Id.Furniture
import Cdda.Monster.Strength

transPlaceMeatSlime :: Strength -> TerFurnTransform
transPlaceMeatSlime s = TerFurnTransform
  { _terFurnTransformId = idTransPlaceMeatSlime s
  , _terFurnTransformFurniture = [furnitureZombie]
  }
    where
      furnitureZombie = TransFurniture
        { _transFurnitureResult         = [ (idFurnitureNull, 89)
                                          , (idMeatSlime s, 1)
                                          ]
        , _transFurnitureValidFurniture = idFurnitureNull
        }

transPlaceMarrowSlime :: Strength -> TerFurnTransform
transPlaceMarrowSlime s = TerFurnTransform
  { _terFurnTransformId = idTransPlaceMarrowSlime s
  , _terFurnTransformFurniture = [furnitureSkeleton]
  }
    where
      furnitureSkeleton = TransFurniture
        { _transFurnitureResult         = [ (idFurnitureNull, 89)
                                          , (idMarrowSlime s, 1)
                                          ]
        , _transFurnitureValidFurniture = idFurnitureNull
        }

allTerFurnTransform :: [TerFurnTransform]
allTerFurnTransform = z ++ s
  where
    z = map transPlaceMeatSlime allZombieStrength
    s = map transPlaceMarrowSlime allSkeletonStrength
