module Define.Harvest where

import Define.Core

data HarvestType = Flesh | Blood | Bone

newtype MassRatio = MassRatio Double

data Entry = Entry Id HarvestType MassRatio

data Harvest = Harvest Id [Entry]
