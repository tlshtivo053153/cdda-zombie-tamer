module Define.TerFurnTransform
  ( TerFurnTransform(..)
  , TransFurniture(..)
  ) where

import Define.Core

data TerFurnTransform = TerFurnTransform
  { _terFurnTransformId :: Id
  , _terFurnTransformFurniture :: [TransFurniture]
  }

data TransFurniture = TransFurniture
  { _transFurnitureResult :: [(Id, Int)]
  , _transFurnitureValidFurniture :: Id
  }
