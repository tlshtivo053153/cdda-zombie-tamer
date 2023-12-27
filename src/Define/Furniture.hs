module Define.Furniture
  ( Furniture(..)
  ) where

import Data.Text
import Define.Core

data Furniture = Furniture
  { _furnitureId :: Id
  , _furnitureName :: Text
  , _furnitureDescription :: Text
  , _furnitureItemGroup :: Id
  }
