module Define.HarvestDropType where

import Define.Core

data HarvestDropType = HarvestDropType
  { _harvestDropTypeId :: Id
  , _harvestDropTypeGroup :: Bool
  }
