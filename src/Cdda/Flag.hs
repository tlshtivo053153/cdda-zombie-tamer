module Cdda.Flag
  ( allFlag
  ) where

import Define.Flag
import Cdda.Flag.Level
import Cdda.Flag.Upgrade

allFlag :: [Flag]
allFlag = concat [ levels, randoms, standards ]
