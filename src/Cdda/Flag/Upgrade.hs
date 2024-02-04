{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Cdda.Flag.Upgrade
  ( randoms
  , getRandom
  , standards
  , getStandard
  ) where

import Define.Core
import Define.Flag
import Define.Monster

import Cdda.Id.Monster

import Data.Maybe
import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Containers.ListUtils as L

randoms :: [Flag]
randoms = L.nubOrd $ map snd $ M.toList randomMonsterMap

random :: UpgradeRandomType -> Flag
random urt = Flag $ T.pack $ "ZT_UPGRADE_RANDOM_" <> show urt

getRandom :: Id -> [Flag]
getRandom monId = maybeToList $ M.lookup monId randomMonsterMap

randomMonsterMap :: M.Map Id Flag
randomMonsterMap = M.fromList $ map (second random)
  [ (,) monZombie URNormal
  , (,) monZombiePrisoner URNormal
  , (,) monZombieTough URNormal
  , (,) monZombieResortDancer URNormal
  , (,) monZombieResortBouncer URNormal
  , (,) monZombieResortStaff URNormal
  , (,) monZombieScientist URNormal
  , (,) monZombieReenactor URNormal
  , (,) monZombieFat URNormal
  , (,) monZombiePrisonerFat URNormal
  , (,) monZombieMedical URMedical
  , (,) monBoomer URBoomer
  , (,) monZombieRust URRust
  , (,) monZombieLabsecurity URLabsecurity
  , (,) monZombieElectric URElectric
  , (,) monZombieTechnician URElectric
  , (,) monSkeleton URSkeleton
  ]

standards :: [Flag]
standards = L.nubOrd $ concatMap snd $ M.toList allUpgradeMap

getStandard :: Id -> [Flag]
getStandard monId = concat $ maybeToList $ M.lookup monId allUpgradeMap

standard :: Id -> Flag
standard monId = Flag $ "ZT_UPGRADE_STANDARD_TO_" <> runId monId

flagUpgradeFromZombie :: Flag
flagUpgradeFromZombieMedic :: Flag

flagUpgradeFromZombie = Flag "ZT_UPGRADE_STANDARD_FROM_ZOMBIE"
flagUpgradeFromZombieMedic = Flag "ZT_UPGRADE_STANDARD_FROM_ZOMBIE_MEDIC"

zombieList :: [Id]
zombieList =
  [ monZombie
  , monZombiePrisoner
  , monZombieTough
  , monZombieResortDancer
  , monZombieResortBouncer
  , monZombieResortStaff
  , monZombieScientist
  , monZombieReenactor
  ]

zombieUpgradeMap :: M.Map Id [Flag]
zombieUpgradeMap = M.fromList $ map (second (:[])) $
  map (,flagUpgradeFromZombie) zombieList
  ++ [ (monZombieMedical, flagUpgradeFromZombieMedic) ]

standardUpgradeTree :: [[Id]]
standardUpgradeTree =
  [ [ monZombieRunner, monZombieHunter, monZombiePredator ]
  , [ monZombieRot, monDevourer ]
  , [ monZombieWretched, monZombieAcidic, monZombieSpitter, monZombieCorrosive ]
  , [ monZombieSwimmerBase, monZombieSwimmer, monZombieMancroc ]
  , [ monZombieStatic, monZombieElectric ]
  , [ monZombieHollow, monShoggoth ]
  , [ monZombieGrabber, monZombieGrappler, monZombieBiter ]
  , [ monZombieSmoker, monSmokerBrute ]
  , [ monZombieShady, monZombieBruteNinja ]
  , [ monZombieShrieker, monZombieScreecher ]
  , [ monZombieNecro, monSkeletonNecro ]
  , [ monZombieBrute, monZombieHulk ]
  , [ monZombieMaster, monSkeletonMaster ]
  , [ monZombieWinged, monZombieBruteWinged ]
  , [ monSkeletonBrute, monSkeletonHulk ]
  ]

standardUpgradeList :: [Flag]
standardUpgradeList = map standard $ concatMap (drop 1) standardUpgradeTree

standardUpgradeMap :: M.Map Id [Flag]
standardUpgradeMap = M.fromList $ map (second (:[])) $ concatMap loop standardUpgradeTree
  where
    loop (x:y:ys) = (x, standard y) : loop (y:ys)
    loop _ = []

allUpgradeMap :: M.Map Id [Flag]
allUpgradeMap = M.unionsWith (++)
  [ zombieUpgradeMap
  , standardUpgradeMap
  ]
