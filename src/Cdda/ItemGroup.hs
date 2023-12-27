module Cdda.ItemGroup where

import Control.Lens

import Define.Core
import Define.ItemGroup
import Define.Monster
import Define.MakeFields

import Cdda.Id.Item
import Cdda.Id.ItemGroup
import Cdda.Monster.Strength

entriesLose :: Id -> [ItemEntry] -> [ItemEntry]
entriesLose itemId es =
  let entryProbSum = sum $ map (^. prob) es
      probLose = 1 - entryProbSum
      lose = ItemEntry itemId probLose
   in if probLose < 0
         then es
         else lose : es

zombieGroup :: Int -> [ItemEntry]
zombieGroup = entriesLose idTaintedMeatPremium . zombieGroup'

zombieGroup' :: Int -> [ItemEntry]
zombieGroup' s = map (\(itemId, p) -> ItemEntry itemId $ p*s')
  [ (,) idTaintedMeatHighPremium 0.0122
  , (,) idTaintedMarrowPremium 0.03
  , (,) idTaintedMarrowHighPremium 0.0006
  ]
    where
      s' = fromIntegral s

skeletonGroup :: Int -> [ItemEntry]
skeletonGroup = entriesLose idTaintedMarrowPremium . skeletonGroup'

skeletonGroup' :: Int -> [ItemEntry]
skeletonGroup' s = map (\(itemId, p) -> ItemEntry itemId $ p*s')
  [ (,) idTaintedMarrowHighPremium 0.001
  ]
    where
      s' = fromIntegral s

allItemGroup :: [ItemGroup]
allItemGroup = zombie ++ skeleton
  where
    zombie = map (\(Strength s) -> ItemGroup (idItemGroupZombie s) $ zombieGroup s) allZombieStrength
    skeleton = map (\(Strength s) -> ItemGroup (idItemGroupSkeleton s)  $ skeletonGroup s) allSkeletonStrength
