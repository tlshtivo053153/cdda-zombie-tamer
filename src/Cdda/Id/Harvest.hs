{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.Harvest where

import qualified Data.Text as T
import Define.Core
import Define.Monster

idHarvestZombie :: Strength -> Id
idHarvestZombie (Strength s) = Id $ T.pack $ "harvest_zombie_strength_" <> show s

idHarvestSkeleton :: Strength -> Id
idHarvestSkeleton (Strength s) = Id $ T.pack $ "harvest_skeleton_strength_" <> show s
