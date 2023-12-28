{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.Harvest
  ( idHarvestZombie
  , idHarvestSkeleton
  ) where

import qualified Data.Text as T
import Define.Core
import Define.Monster

idHarvestZombie :: Strength -> Id
idHarvestZombie s = Id $ T.pack $ "harvest_zombie_strength_" <> show s

idHarvestSkeleton :: Strength -> Id
idHarvestSkeleton s = Id $ T.pack $ "harvest_skeleton_strength_" <> show s
