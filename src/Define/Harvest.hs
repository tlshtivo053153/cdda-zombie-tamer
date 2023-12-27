module Define.Harvest
  ( HarvestType(..)
  , Entry(..)
  , Harvest(..)
  ) where

import Define.Core

data HarvestType = Flesh | Blood | Bone | TaintedFood

data Entry = EntryRatio Id HarvestType Double
           | EntryBase Id HarvestType (Int, Int) (Double, Double)
           | EntryDrop Id HarvestType

data Harvest = Harvest Id [Entry]
