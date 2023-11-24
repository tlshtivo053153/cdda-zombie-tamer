module Define.Harvest where

import Define.Core

data HarvestType = Flesh | Blood | Bone

data Entry = EntryRatio Id HarvestType Double
           | EntryBase Id HarvestType (Int, Int) (Double, Double)

data Harvest = Harvest Id [Entry]
