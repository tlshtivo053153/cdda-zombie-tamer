{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Define.Item.Petfood
  ( ItemPetfood(..)
  ) where

import Define.Core

import GHC.Generics (Generic)
import Data.Default

data ItemPetfood = ItemPetfood
    { _itemPetfoodCopyFrom :: Id
    , _itemPetfoodId :: Id
    , _itemPetfoodName :: Name
    , _itemPetfoodDescription :: Description
    , _itemPetfoodUseAction :: [UseAction]
    , _itemPetfoodPetfood :: [FoodCategory]
    }
    deriving (Generic, Default)
