module Define.Furniture where

import Data.Text
import Define.Core

data Furniture = Furnitrue
  { _furnitureId :: Id
  , _furnitureName :: Text
  , _furnitureDescription :: Text
  , _furnitureItemGroup :: Id
  }
