{-# LANGUAGE OverloadedStrings #-}
module Cdda.Flag.Monster
  ( allIsMonster
  , isMonster
  ) where

import Define.Core
import Define.Flag

import Cdda.Id.Monster

allIsMonster :: [Flag]
allIsMonster = map isMonster allFriendMonster

isMonster :: Id -> Flag
isMonster (Id baseMonsterId) = Flag $ "ZT_MON_IS_" <> baseMonsterId
