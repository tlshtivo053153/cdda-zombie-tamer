module Cdda.Flag
  ( allFlag
  ) where

import Define.Flag
import Cdda.Flag.Level
import Cdda.Flag.Upgrade
import Cdda.Flag.Monster

allFlag :: [Flag]
allFlag = concat [ levels, randoms, standards, allIsMonster ]
