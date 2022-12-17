module Define.Item where

import Data.Text

import Define.Core

newtype UseAction = UseAction Text

data Item = Item
    { _itemCopyFrom :: Id
    , _itemId :: Id
    , _itemName :: Name
    , _itemDescription :: Description
    , _itemUseAction :: [UseAction]
    , _itemPetfood :: [FoodCategory]
    }
