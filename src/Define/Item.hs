{-# LANGUAGE DeriveGeneric #-}
module Define.Item
  ( UseAction(..)
  , Item(..)
  ) where

import GHC.Generics (Generic)
import Data.Text
import Data.Aeson

import Define.Aeson
import Define.Core

newtype UseAction = UseAction Text
  deriving Generic

instance ToJSON UseAction where
  toJSON = genericToJSON cddaOption

data Item = Item
    { _itemCopyFrom :: Id
    , _itemId :: Id
    , _itemName :: Name
    , _itemDescription :: Description
    , _itemUseAction :: [UseAction]
    , _itemPetfood :: [FoodCategory]
    }
