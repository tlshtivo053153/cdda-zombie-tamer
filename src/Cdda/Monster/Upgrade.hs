{-# LANGUAGE TupleSections #-}
module Cdda.Monster.Upgrade
  ( getUpgradeRandom
  , getUpgradeStandard
  ) where

import Define.Monster
import Define.Core

import Cdda.Id.Monster
import Cdda.Id.MonsterGroup
import Cdda.Id.Item

import Data.Maybe
import Data.Bifunctor
import qualified Data.Map as M

randomUpgradeNormal :: UpgradeRandom
randomUpgradeNormal = UpgradeRandom (UCHaveItem idTaintedMeatPremium 1) URNormal

randomUpgradeFat :: UpgradeRandom
randomUpgradeFat = UpgradeRandom (UCHaveItem idTaintedMeatPremium 1) URFat

randomUpgradeMedical :: UpgradeRandom
randomUpgradeMedical = UpgradeRandom (UCHaveItem idTaintedMeatPremium 2) URMedical

randomUpgradeBoomer :: UpgradeRandom
randomUpgradeBoomer = UpgradeRandom (UCHaveItem idTaintedMeatPremium 1) URBoomer

randomUpgradeRust :: UpgradeRandom
randomUpgradeRust = UpgradeRandom (UCHaveItem idTaintedMeatPremium 1) URRust

randomUpgradeLabsecurity :: UpgradeRandom
randomUpgradeLabsecurity = UpgradeRandom (UCHaveItem idTaintedMeatPremium 2) URLabsecurity

randomUpgradeElectric :: UpgradeRandom
randomUpgradeElectric = UpgradeRandom (UCHaveItem idTaintedMeatHighPremium 1) URElectric

randomUpgradeSkeleton :: UpgradeRandom
randomUpgradeSkeleton = UpgradeRandom (UCHaveItem idTaintedMarrow 1) URSkeleton

randomUpgradeNone :: UpgradeRandom
randomUpgradeNone = UpgradeRandom UCFalse URNone

randomMonsterMap :: M.Map Id UpgradeRandom
randomMonsterMap = M.fromList
  [ (,) monZombie randomUpgradeNormal
  , (,) monZombiePrisoner randomUpgradeNormal
  , (,) monZombieTough randomUpgradeNormal
  , (,) monZombieResortDancer randomUpgradeNormal
  , (,) monZombieResortBouncer randomUpgradeNormal
  , (,) monZombieResortStaff randomUpgradeNormal
  , (,) monZombieScientist randomUpgradeNormal
  , (,) monZombieReenactor randomUpgradeNormal
  , (,) monZombieFat randomUpgradeFat
  , (,) monZombiePrisonerFat randomUpgradeFat
  , (,) monZombieMedical randomUpgradeMedical
  , (,) monBoomer randomUpgradeBoomer
  , (,) monZombieRust randomUpgradeRust
  , (,) monZombieLabsecurity randomUpgradeLabsecurity
  , (,) monZombieElectric randomUpgradeElectric
  , (,) monZombieTechnician randomUpgradeElectric
  , (,) monSkeleton randomUpgradeSkeleton
  ]

getUpgradeRandom :: Id -> UpgradeRandom
getUpgradeRandom monId =
  fromMaybe randomUpgradeNone $ M.lookup monId randomMonsterMap

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

zombieUpgradeList :: [UpgradeStandard]
zombieUpgradeList = map (\(x,y) -> UpgradeStandard y x)
  [ (,) monGasZombie $ UCHaveItem idGasoline 60000
  , (,) monZombieKevlar1 $ UCHaveItem idSheetKevlar 500
  , (,) monSkeleton $ UCHaveItem idMealBone 200
  , (,) monZombieAcidic $ UCHaveItem idChemSulphuricAcid 2
  , (,) monZombieRust $ UCHaveItem idScrap 1000
  , (,) monZombieThorny $ UCHaveItem idVeggy 100
  , (,) monZombieStatic $ UCHaveItem idBurntOutBionic 1
  ]

zombieUpgradeMap :: M.Map Id [UpgradeStandard]
zombieUpgradeMap = M.fromList $ map (,zombieUpgradeList) zombieList

zombieMedicUpgradeList :: [UpgradeStandard]
zombieMedicUpgradeList = map (\(x,y) -> UpgradeStandard y x)
  [ (,) monGasZombie $ UCHaveItem idGasoline 60000
  , (,) monZombieKevlar1 $ UCHaveItem idSheetKevlar 500
  , (,) monSkeletonMedical $ UCHaveItem idMealBone 200
  , (,) monZombieMedicalAcidic $ UCHaveItem idChemSulphuricAcid 1
  , (,) monZombieRust $ UCHaveItem idScrap 1000
  , (,) monZombieThorny $ UCHaveItem idVeggy 100
  , (,) monZombieStatic $ UCHaveItem idBurntOutBionic 1
  ]

zombieMedicUpgradeMap :: M.Map Id [UpgradeStandard]
zombieMedicUpgradeMap = M.singleton monZombieMedical zombieMedicUpgradeList

standardUpgradeConfig :: M.Map Id UpgradeCondition
standardUpgradeConfig = M.fromList
  [ (,) monZombieRunner UCFalse
  , (,) monZombieHunter $ UCHaveItem idTaintedMeatPremium 10
  , (,) monZombiePredator $ UCHaveItem idTaintedMeatHighPremium 1
  , (,) monZombieRot UCFalse
  , (,) monDevourer $ UCHaveItem idTaintedMeatHighPremium 1
  , (,) monZombieWretched UCFalse
  , (,) monZombieAcidic $ UCHaveItem idTaintedMeatPremium 1
  , (,) monZombieSpitter $ UCHaveItem idTaintedMeatPremium 10
  , (,) monZombieCorrosive $ UCHaveItem idTaintedMeatHighPremium 1
  , (,) monZombieSwimmerBase UCFalse
  , (,) monZombieSwimmer $ UCHaveItem idTaintedMeatPremium 1
  , (,) monZombieMancroc $ UCHaveItem idTaintedMeatPremium 1
  , (,) monZombieStatic UCFalse
  , (,) monZombieElectric $ UCHaveItem idTaintedMeatPremium 1
  , (,) monZombieHollow UCFalse
  , (,) monShoggoth $ UCHaveItem idTaintedMarrowHighPremium 1
  , (,) monZombieGrabber UCFalse
  , (,) monZombieGrappler $ UCHaveItem idTaintedMeatPremium 5
  , (,) monZombieBiter $ UCHaveItem idTaintedMeatPremium 10
  , (,) monZombieSmoker UCFalse
  , (,) monSmokerBrute $ UCHaveItem idTaintedMeatPremium 5
  , (,) monZombieShady UCFalse
  , (,) monZombieBruteNinja $ UCHaveItem idTaintedMeatPremium 20
  , (,) monZombieShrieker UCFalse
  , (,) monZombieScreecher $ UCHaveItem idTaintedMeatPremium 5
  , (,) monZombieNecro UCFalse
  , (,) monSkeletonNecro $ UCHaveItem idTaintedMarrowPremium 1
  , (,) monZombieBrute UCFalse
  , (,) monZombieHulk $ UCHaveItem idTaintedMeatHighPremium 1
  , (,) monZombieMaster UCFalse
  , (,) monSkeletonMaster $ UCHaveItem idTaintedMarrowPremium 1
  , (,) monZombieWinged UCFalse
  , (,) monZombieBruteWinged $ UCHaveItem idTaintedMeatPremium 10
  , (,) monSkeletonBrute UCFalse
  , (,) monSkeletonHulk $ UCHaveItem idTaintedMeatPremium 1
  ]

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

standardUpgradeMap :: M.Map Id [UpgradeStandard]
standardUpgradeMap = M.fromList $ map (second $ \(x, y) -> [UpgradeStandard y x])
                                $ concatMap loop standardUpgradeTree
  where
    loop (x:y:ys) =  case M.lookup y standardUpgradeConfig of
         Just v -> (x, (y, v)) : loop (y:ys)
         Nothing -> loop (y:ys)
    loop _ = []

allUpgradeMap :: M.Map Id [UpgradeStandard]
allUpgradeMap = M.unionsWith (++)
  [ zombieUpgradeMap
  , zombieMedicUpgradeMap
  , standardUpgradeMap
  ]

getUpgradeStandard :: Id -> [UpgradeStandard]
getUpgradeStandard monId =
  fromMaybe [] $ M.lookup monId allUpgradeMap
