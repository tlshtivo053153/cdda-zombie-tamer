module Cdda.Monster.Upgrade
  ( allUpgradeRandomType
  , allUpgradeRandomList
  , allUpgradeStandardList
  ) where

import Define.Monster
import Define.Flag

import Cdda.Id.Monster
import Cdda.Id.Item
import Cdda.Flag.Upgrade

allUpgradeRandomType :: [UpgradeRandomType]
allUpgradeRandomType =
  [ URNormal
  , URFat
  , URMedical
  , URBoomer
  , URRust
  , URLabsecurity
  , URElectric
  , URSkeleton
  , URNone
  ]

allUpgradeRandomList :: [(UpgradeRandom, [Flag])]
allUpgradeRandomList = map (\(x,y,z) -> (UpgradeRandom (UCHaveItem x y) z, [random z]))
  [ (,,) idTaintedMeatPremium 1 URNormal
  , (,,) idTaintedMeatPremium 1 URFat
  , (,,) idTaintedMeatPremium 2 URMedical
  , (,,) idTaintedMeatPremium 1 URBoomer
  , (,,) idTaintedMeatPremium 1 URRust
  , (,,) idTaintedMeatPremium 2 URLabsecurity
  , (,,) idTaintedMeatPremium 1 URElectric
  , (,,) idTaintedMarrowPremium 1 URSkeleton
  ]

zombieUpgradeList :: [(UpgradeStandard, [Flag])]
zombieUpgradeList = map f
  [ (,) monGasZombie $ UCHaveItem idGasoline 60000
  , (,) monZombieKevlar1 $ UCHaveItem idSheetKevlar 500
  , (,) monZombieRust $ UCHaveItem idScrap 1000
  , (,) monZombieThorny $ UCHaveItem idVeggy 100
  , (,) monZombieStatic $ UCHaveItem idBurntOutBionic 1
  ]
    where
      f (x, y) = (UpgradeStandard y x, flags)
      flags = [ flagUpgradeFromZombie, flagUpgradeFromZombieMedic ]

zombieNormalUpgradeList :: [(UpgradeStandard, [Flag])]
zombieNormalUpgradeList = map f
  [ (,) monSkeleton $ UCHaveItem idMealBone 200
  , (,) monZombieAcidic $ UCHaveItem idChemSulphuricAcid 2
  ]
    where
      f (x, y) = (UpgradeStandard y x, [flagUpgradeFromZombie])

zombieMedicUpgradeList :: [(UpgradeStandard, [Flag])]
zombieMedicUpgradeList = map f
  [ (,) monSkeletonMedical $ UCHaveItem idMealBone 200
  , (,) monZombieMedicalAcidic $ UCHaveItem idChemSulphuricAcid 1
  ]
    where
      f (x, y) = (UpgradeStandard y x, [flagUpgradeFromZombieMedic])

standardUpgradeList :: [(UpgradeStandard, [Flag])]
standardUpgradeList = map f
  [ (,) monZombieHunter $ UCHaveItem idTaintedMeatPremium 10
  , (,) monZombiePredator $ UCHaveItem idTaintedMeatHighPremium 1
  , (,) monDevourer $ UCHaveItem idTaintedMeatHighPremium 1
  , (,) monZombieAcidic $ UCHaveItem idTaintedMeatPremium 1
  , (,) monZombieSpitter $ UCHaveItem idTaintedMeatPremium 10
  , (,) monZombieCorrosive $ UCHaveItem idTaintedMeatHighPremium 1
  , (,) monZombieSwimmer $ UCHaveItem idTaintedMeatPremium 1
  , (,) monZombieMancroc $ UCHaveItem idTaintedMeatPremium 1
  , (,) monZombieElectric $ UCHaveItem idTaintedMeatPremium 1
  , (,) monShoggoth $ UCHaveItem idTaintedMarrowHighPremium 1
  , (,) monZombieGrappler $ UCHaveItem idTaintedMeatPremium 5
  , (,) monZombieBiter $ UCHaveItem idTaintedMeatPremium 10
  , (,) monSmokerBrute $ UCHaveItem idTaintedMeatPremium 5
  , (,) monZombieBruteNinja $ UCHaveItem idTaintedMeatPremium 20
  , (,) monZombieScreecher $ UCHaveItem idTaintedMeatPremium 5
  , (,) monSkeletonNecro $ UCHaveItem idTaintedMarrowPremium 1
  , (,) monZombieHulk $ UCHaveItem idTaintedMeatHighPremium 1
  , (,) monSkeletonMaster $ UCHaveItem idTaintedMarrowPremium 1
  , (,) monZombieBruteWinged $ UCHaveItem idTaintedMeatPremium 10
  , (,) monSkeletonHulk $ UCHaveItem idTaintedMeatPremium 1
  ]
    where
      f (x, y) = (UpgradeStandard y x, [standard x])

allUpgradeStandardList :: [(UpgradeStandard, [Flag])]
allUpgradeStandardList = concat
  [ zombieUpgradeList
  , zombieNormalUpgradeList
  , zombieMedicUpgradeList
  , standardUpgradeList
  ]
