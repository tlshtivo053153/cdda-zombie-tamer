module Cdda.HarvestDropType where

import Define.HarvestDropType

import Cdda.Id.HarvestDropType

harvestDropTypeTaintedFood :: HarvestDropType
harvestDropTypeTaintedFood = HarvestDropType
  { _harvestDropTypeId    = idHarvestDropTypeTaintedFood
  , _harvestDropTypeGroup = True
  }
