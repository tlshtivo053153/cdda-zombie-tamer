module Define.ItemGroup where

import Define.Core

data ItemGroup = ItemGroup
  { _itemGroupId :: Id
  , _itemGroupEntries :: [ItemEntry]
  }

data ItemEntry = ItemEntry
  { _itemEntryItem :: Id
  , _itemEntryProb :: Rational
  }
