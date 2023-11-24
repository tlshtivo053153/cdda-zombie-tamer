{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Cdda.Monster.Growth
  ( getGrowth ) where

import Prelude hiding ( exp )

import Define.Core
import Define.Monster
import Define.MakeFields

import Cdda.Id.Monster

import qualified Data.Map as M

import Control.Lens

noGrowth :: Growth
noGrowth = Growth 1 0

noArmorGrowth :: ArmorGrowth
noArmorGrowth = ArmorGrowth
  { _armorGrowthBash = noGrowth
  , _armorGrowthBullet = noGrowth
  , _armorGrowthCut = noGrowth
  , _armorGrowthStab = noGrowth
  , _armorGrowthAcid = noGrowth
  , _armorGrowthFire = noGrowth
  , _armorGrowthElec = noGrowth
  , _armorGrowthCold = noGrowth
  , _armorGrowthPure = noGrowth
  }

noDamageGrowth :: DamageGrowth
noDamageGrowth = DamageGrowth
  { _damageGrowthDamageType = ""
  , _damageGrowthAmount = noGrowth
  , _damageGrowthArmorPenetration = noGrowth
  }

noMeleeGrowth :: MeleeGrowth
noMeleeGrowth = MeleeGrowth
  { _meleeGrowthSkill = noGrowth
  , _meleeGrowthDice = noGrowth
  , _meleeGrowthDiceSides = noGrowth
  , _meleeGrowthDamage = []
  }

noStatusGrowth :: StatusGrowth
noStatusGrowth = StatusGrowth
  { _statusGrowthHp = noGrowth
  , _statusGrowthSpeed = noGrowth
  , _statusGrowthDodge = noGrowth
  , _statusGrowthArmor = noArmorGrowth
  , _statusGrowthMelee = noMeleeGrowth
  , _statusGrowthRegenerates = noGrowth
  , _statusGrowthExp = noGrowth
  , _statusGrowthMaxLevel = 1
  }

zombieNormalGrowth :: StatusGrowth
zombieNormalGrowth = noStatusGrowth
  & hp .~ Growth 1.1 0
  & speed .~ Growth 1.05 1
  & dodge .~ Growth 1 0.1
  & melee.skill .~ Growth 1 0.1
  & melee.dice .~ Growth 1 0.2
  & melee.diceSides .~ Growth 1 0.2
  & melee.damage .~ [ noDamageGrowth
      & damageType .~ "cut"
      & amount .~ Growth 1.05 0
      & armorPenetration .~ Growth 1 0.1
                    ]
  & exp .~ Growth 1.12 0
  & maxLevel .~ 30

zombieBruteGrowth :: StatusGrowth
zombieBruteGrowth = zombieNormalGrowth
  & hp .~ Growth 1.15 0
  & dodge .~ Growth 1 0.2
  & melee.skill .~ Growth 1 0.2
  & melee.dice .~ Growth 1 0.3
  & melee.damage .~ [ noDamageGrowth
      & damageType .~ "cut"
      & amount .~ Growth 1.05 0
      & armorPenetration .~ Growth 1 0.3
                    ]
  & exp .~ Growth 1.18 0

zombieHulkGrowth :: StatusGrowth
zombieHulkGrowth = zombieNormalGrowth
  & dodge .~ Growth 1 0.3
  & melee.skill .~ Growth 1 0.3
  & melee.dice .~ Growth 1 0.4
  & melee.diceSides .~ Growth 1 0.3
  & melee.damage .~ [ noDamageGrowth
      & damageType .~ "cut"
      & amount .~ Growth 1 0.4
      & armorPenetration .~ Growth 1 0.4
                    ]
  & armor.bash .~ Growth 1.05 0
  & armor.cut .~ Growth 1.05 0
  & armor.bullet .~ Growth 1.05 0
  & armor.elec .~ Growth 1.05 0
  & exp .~ Growth 1.24 0

skeletonGrowth :: StatusGrowth
skeletonGrowth = noStatusGrowth
  & hp .~ Growth 1.2 0
  & speed .~ Growth 1.05 1
  & dodge .~ Growth 1 0.1
  & melee.skill .~ Growth 1 0.1
  & melee.dice .~ Growth 1 0.2
  & melee.diceSides .~ Growth 1 0.3
  & melee.damage .~ [ noDamageGrowth
      & damageType .~ "cut"
      & amount .~ Growth 1.05 0
      & armorPenetration .~ Growth 1 0.2
                    ]
  & armor.bash .~ Growth 1 1
  & armor.cut .~ Growth 1 2
  & armor.bullet .~ Growth 1 1
  & armor.elec .~ Growth 1 0.5
  & exp .~ Growth 1.18 0
  & maxLevel .~ 30

skeletonBruteGrowth :: StatusGrowth
skeletonBruteGrowth = skeletonGrowth
  & hp .~ Growth 1.1 0
  & speed .~ Growth 1.05 1
  & melee.dice .~ Growth 1.1 0.2
  & melee.diceSides .~ Growth 1 0.3
  & melee.damage .~ [ noDamageGrowth
      & damageType .~ "cut"
      & amount .~ Growth 1.05 0
      & armorPenetration .~ Growth 1 0.2
                    ]
  & armor.bash .~ Growth 1.1 0
  & armor.cut .~ Growth 1.1 0
  & armor.bullet .~ Growth 1.1 0
  & armor.elec .~ Growth 1.1 0.1
  & exp .~ Growth 1.2 0

skeletonHulkGrowth :: StatusGrowth
skeletonHulkGrowth = skeletonGrowth
  & hp .~ Growth 1.05 0
  & speed .~ Growth 1.05 1
  & dodge .~ noGrowth
  & melee.dice .~ Growth 1 0.2
  & melee.diceSides .~ Growth 1 0.3
  & melee.damage .~ [ noDamageGrowth
      & damageType .~ "cut"
      & amount .~ Growth 1.05 0
      & armorPenetration .~ Growth 1 1
                    ]
  & armor.bash .~ Growth 1.1 0
  & armor.cut .~ Growth 1.1 0
  & armor.bullet .~ Growth 1.1 0
  & armor.elec .~ Growth 1 0.5
  & exp .~ Growth 1.3 0

zombieRegeneratingGrowth :: StatusGrowth
zombieRegeneratingGrowth = zombieNormalGrowth
  & hp .~ Growth 1.05 0
  & regenerates .~ Growth 1.1 0

zombieCopGrowth :: StatusGrowth
zombieCopGrowth = zombieNormalGrowth
  & hp .~ Growth 1.15 0
  & dodge .~ Growth 1 0.2

zombieMedicalGrowth :: StatusGrowth
zombieMedicalGrowth = zombieNormalGrowth
  & armor.elec .~ Growth 1.05 1
  & armor.acid .~ Growth 1 1

zombieMedicalBruteGrowth :: StatusGrowth
zombieMedicalBruteGrowth = zombieBruteGrowth
  & armor.elec .~ Growth 1.05 1
  & armor.acid .~ Growth 1 1

zombieMedicalRegeneratingGrowth :: StatusGrowth
zombieMedicalRegeneratingGrowth = zombieRegeneratingGrowth
  & armor.elec .~ Growth 1.05 1
  & armor.acid .~ Growth 1 1

boomerGrowth :: StatusGrowth
boomerGrowth = noStatusGrowth
  & hp .~ Growth 1.3 0
  & melee.skill .~ Growth 1 0.1
  & melee.dice .~ Growth 1 0.2
  & melee.diceSides .~ Growth 1 0.2
  & melee.damage .~ [ noDamageGrowth
      & damageType .~ "cut"
      & amount .~ Growth 1 1
      & armorPenetration .~ Growth 1 0.1
                    ]
  & exp .~ Growth 1.12 0
  & maxLevel .~ 30

boomerGluttonGrowth :: StatusGrowth
boomerGluttonGrowth = boomerGrowth
  & armor.bash .~ Growth 1.1 0
  & armor.cut .~ Growth 1.1 0
  & armor.bullet .~ Growth 1.1 0
  & armor.elec .~ Growth 1.1 0
  & exp .~ Growth 1.18 0

zombieStaticGrowth :: StatusGrowth
zombieStaticGrowth = zombieNormalGrowth
  & melee.damage .~ [ noDamageGrowth
      & damageType .~ "electric"
      & amount .~ Growth 1.1 0
                    ]

zombieBruteShockerGrowth :: StatusGrowth
zombieBruteShockerGrowth = zombieBruteGrowth
  & melee.damage .~ [ noDamageGrowth
      & damageType .~ "electric"
      & amount .~ Growth 1.1 0
                    , noDamageGrowth
      & damageType .~ "cut"
      & amount .~ Growth 1.1 0
                    ]

zombieNullfieldGrowth :: StatusGrowth
zombieNullfieldGrowth = zombieStaticGrowth
  & speed .~ noGrowth
  & melee.damage .~ [ noDamageGrowth
      & damageType .~ "electric"
      & amount .~ Growth 1.15 0
                    ]
  & exp .~ Growth 1.18 0

shoggothGrowth :: StatusGrowth
shoggothGrowth = noStatusGrowth
  & hp .~ Growth 1.1 0
  & speed .~ Growth 1.05 1
  & melee.dice .~ Growth 1 0.5
  & melee.diceSides .~ Growth 1 0.3
  & melee.damage .~ [ noDamageGrowth
      & damageType .~ "acid"
      & amount .~ Growth 1.1 0
      & armorPenetration .~ Growth 1 1
                    , noDamageGrowth
      & damageType .~ "cut"
      & amount .~ Growth 1 1
      & armorPenetration .~ Growth 1 0.5
                    ]
  & armor.bash .~ Growth 1.1 0
  & armor.cut .~ Growth 1.1 0
  & armor.bullet .~ Growth 1.1 0
  & armor.elec .~ Growth 1.1 0
  & regenerates .~ Growth 1.1 0
  & exp .~ Growth 1.3 0
  & maxLevel .~ 50

zombieNormalMap :: [Id]
zombieNormalMap =
  [ monZombieAcidic
  , monZombieWretched
  , monZombieDogAcidic
  , monZombieDogBruteAcidic
  , monMeatCocoonTiny
  , monMeatCocoonSmall
  , monMeatCocoonMed
  , monMeatCocoonLarge
  , monAmalgamationSwarmer
  , monAmalgamationSpotter
  , monAmalgamationCorroder
  , monAmalgamationJumper
  , monAmalgamationZapper
  , monAmalgamationSoldier
  , monZombieChildScorched
  , monZombieFiend
  , monZombieScorched
  , monZombieAnklebiter
  , monZombieChild
  , monZombieCreepy
  , monZombieShriekling
  , monZombieSnotgobbler
  , monZombieSproglodyte
  , monZombieWaif
  , monZombieWretch
  , monZombieRust
  , monZombieShell
  , monZombiePlated
  , monZombieUrchin
  , monZombieHammerHands
  , monCrawler
  , monDevourerLabSec
  , monZombieCrushedGiant
  , monZombieScissorlimbs
  , monZombieHangingInnards
  , monZombieGiantHeart
  , monZombieLivingWall
  , monZombieScientist
  , monZombieLabsecurity
  , monZombiePhaseSkulker
  , monZombiePhaseShrike
  , monFrogMother
  , monTadpoleGrabber
  , monZombullfrog
  , monZombieBiter
  , monZombieBrainless
  , monAfsHeadlessHorror
  , monZombieEars
  , monZombieNemesis
  , monZombieGrabber
  , monZombieGrappler
  , monZombieHollow
  , monZombieJackson
  , monZombieMaster
  , monZombieNecro
  , monZombieNecroBoomer
  , monZombieRunner
  , monZombieScreecher
  , monZombieShady
  , monShia
  , monZombieShrieker
  , monZombieSkull
  , monZombieSmoker
  , monZombieSwimmerBase
  , monZombieSwimmer
  , monZombieTechnician
  , monZombieMiner
  , monZombieThorny
  , monZombieReenactor
  , monZeriatric
  , monZombiePrisoner
  , monZombiePrisonerFat
  , monZombiePrisonerTough
  , monCharredNightmare
  , monIrradiatedWanderer1
  , monIrradiatedWanderer2
  , monIrradiatedWanderer3
  , monIrradiatedWanderer4
  , monZombieSoldier
  , monZombieSoldierBlackops1
  , monZombieSoldierBlackops2
  , monZombieSoldierAcid1
  , monZombieSoldierAcid2
  , monZombieKevlar1
  , monZombieKevlar2
  , monZombieMilitaryPilot
  , monZombieMilbasePersonnel
  , monZombieSailor
  , monZombieOfficer
  , monZombieMarine
  , monZombieMarineUpgrade
  , monZombieFlamer
  , monZombieArmored
  , monZombieBioOp
  , monZombieBioOp2
  , monZombieSurvivor
  , monZombieSurvivorElite
  , monZombieMedicalBrute
  , monZombieCrawlerPupaDecoy
  , monZombieCrawlerPupa
  , monZombiePupaDecoy
  , monZombiePupa
  , monZombiePupaDecoyShady
  , monZombiePupaShady
  , monBeekeeper
  , monZombie
  , monZombieCrawler
  , monZombieDancer
  , monZombieFat
  , monZombieFireman
  , monZombieHazmat
  , monZombieRot
  , monZombieSwat
  , monZombieTough
  , monZombieResortDancer
  , monZombieResortBouncer
  , monZombieResortStaff
  , monZombieWinged
  , monSpawnRaptor
  , monSpawnRaptorShady
  , monSpawnRaptorUnstable
  , monSpawnRaptorElectric
  , monShoggoth
  , monZhark
  , monZombieDog
  , monDogZombieCop
  , monDogZombieRot
  , monZolf
  , monZombear
  , monZombiePig
  , monZombeaver
  , monZoose
  , monZougar
  , monZombieHorse
  , monZiger
  , monZow
  , monZombull
  , monZllama
  , monZeer
  , monZpiderMass
  , monZeindeer
  , monGastroBufo
  ]

zombieBruteMap :: [Id]
zombieBruteMap =
  [ monZombieBrute
  , monZombieBruteGrappler
  , monZombieBruteNinja
  , monSmokerBrute
  , monZombiePrisonerBrute
  , monBrutePupaDecoy
  , monBrutePupa
  , monZombieBruteWinged
  , monZombieHunter
  , monZombiePredator
  , monZombieMancroc
  , monZombieSpitter
  ]

zombieHulkMap :: [Id]
zombieHulkMap =
  [ monZombieHulk
  , monHulkPupaDecoy
  , monHulkPupa
  , monDevourer
  , monZombieCorrosive
  ]

skeletonMap :: [Id]
skeletonMap =
  [ monSkeleton
  , monSkeletonMedical
  ]

skeletonBruteMap :: [Id]
skeletonBruteMap =
  [ monSkeletonBrute
  , monSkeletonElectric
  ]

skeletonHulkMap :: [Id]
skeletonHulkMap =
  [ monSkeletonMaster
  , monSkeletonNecro
  , monSkeletonHulk
  ]

zombieRegeneratingMap :: [Id]
zombieRegeneratingMap = [ monZombieRegenerating ]

zombieCopMap :: [Id]
zombieCopMap = [ monZombieCop ]

zombieMedicalMap :: [Id]
zombieMedicalMap =
  [ monZombieMedical
  , monZombieMedicalPupa
  , monZombiePupaMedicalDecoy
  , monZombieMedicalAcidic
  ]

zombieMedicalBruteMap :: [Id]
zombieMedicalBruteMap = [ monZombieMedicalBrute ]

zombieMedicalRegeneratingMap :: [Id]
zombieMedicalRegeneratingMap = [ monZombieMedicalRegenerating ]

boomerMap :: [Id]
boomerMap =
  [ monBoomer
  , monBoomerHuge
  , monBoomerClaymore
  , monGasZombie
  , monZombieGasbag
  , monZombieRelaxGasbag
  , monZombieTearGasbag
  , monZombieGasbagImmobile
  , monZombieGasbagCrawler
  , monZombieGasbagImpaler
  ]

boomerGluttonMap :: [Id]
boomerGluttonMap = [ monBoomerGlutton ]

zombieStaticMap :: [Id]
zombieStaticMap =
  [ monZombieStatic
  , monZombieElectric
  ]

zombieBruteShockerMap :: [Id]
zombieBruteShockerMap = [ monZombieBruteShocker ]

zombieNullfieldMap :: [Id]
zombieNullfieldMap = [ monZombieNullfield ]

shoggothMap :: [Id]
shoggothMap = [ monShoggoth ]

getGrowth :: Id -> Maybe StatusGrowth
getGrowth monId = M.lookup monId $ M.fromList
                                 $ concatMap (\(m, g) -> map (,g) m)
  [ (,) zombieNormalMap              zombieNormalGrowth
  , (,) zombieBruteMap               zombieBruteGrowth
  , (,) zombieHulkMap                zombieHulkGrowth
  , (,) skeletonMap                  skeletonGrowth
  , (,) skeletonBruteMap             skeletonBruteGrowth
  , (,) skeletonHulkMap              skeletonHulkGrowth
  , (,) zombieRegeneratingMap        zombieRegeneratingGrowth
  , (,) zombieCopMap                 zombieCopGrowth
  , (,) zombieMedicalMap             zombieMedicalGrowth
  , (,) zombieMedicalBruteMap        zombieMedicalBruteGrowth
  , (,) zombieMedicalRegeneratingMap zombieMedicalRegeneratingGrowth
  , (,) boomerMap                    boomerGrowth
  , (,) boomerGluttonMap             boomerGluttonGrowth
  , (,) zombieStaticMap              zombieStaticGrowth
  , (,) zombieBruteShockerMap        zombieBruteShockerGrowth
  , (,) zombieNullfieldMap           zombieNullfieldGrowth
  , (,) shoggothMap                  shoggothGrowth
  ]
