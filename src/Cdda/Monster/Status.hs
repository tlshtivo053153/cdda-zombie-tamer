{-# LANGUAGE OverloadedStrings #-}
module Cdda.Monster.Status
  ( getStatus
  , statusWithLevel
  ) where

import Prelude hiding ( pure, id )
import Define.Core
import Define.Monster
import Define.MakeFields

import Cdda.Id.Monster

import Data.Maybe
import qualified Data.Map as M
import Control.Lens

emptyDamage :: Damage
emptyDamage = Damage
  { _damageDamageType = ""
  , _damageAmount = 0
  , _damageArmorPenetration = 0
  }

emptyStatus :: Status
emptyStatus = Status
  { _statusHp = 0
  , _statusSpeed = 0
  , _statusDodge = 0
  , _statusArmor = Armor 
      { _armorBash = 0
      , _armorBullet = 0
      , _armorCut = 0
      , _armorStab = 0
      , _armorAcid = 0
      , _armorHeat = 0
      , _armorElec = 0
      , _armorCold = 0
      , _armorPure = 0
      }
  , _statusMelee = Melee
      { _meleeSkill = 0
      , _meleeDice = 0
      , _meleeDiceSides = 0
      , _meleeDamage = []
      }
  , _statusRegenerates = 0
  }

statusMonZhark :: Status
statusMonZombieDog :: Status
statusMonDogZombieCop :: Status
statusMonDogZombieRot :: Status
statusMonZolf :: Status
statusMonZombear :: Status
statusMonZombiePig :: Status
statusMonZombeaver :: Status
statusMonZoose :: Status
statusMonZougar :: Status
statusMonZombieHorse :: Status
statusMonZiger :: Status
statusMonZow :: Status
statusMonZombull :: Status
statusMonZllama :: Status
statusMonZeer :: Status
statusMonZpiderMass :: Status
statusMonZeindeer :: Status
statusMonGastroBufo :: Status
statusMonBeekeeper :: Status
statusMonZombie :: Status
statusMonZombieCop :: Status
statusMonZombieCrawler :: Status
statusMonZombieDancer :: Status
statusMonZombieFat :: Status
statusMonZombieFireman :: Status
statusMonZombieHazmat :: Status
statusMonZombieRot :: Status
statusMonZombieSwat :: Status
statusMonZombieTough :: Status
statusMonZombieResortDancer :: Status
statusMonZombieResortBouncer :: Status
statusMonZombieResortStaff :: Status
statusMonZombieMedical :: Status
statusMonZombieMedicalBrute :: Status
statusMonZombieMedicalRegenerating :: Status
statusMonSkeletonMedical :: Status
statusMonZombieMedicalAcidic :: Status
statusMonZombieMedicalPupa :: Status
statusMonZombiePupaMedicalDecoy :: Status
statusMonZombieCrawlerPupaDecoy :: Status
statusMonZombieCrawlerPupa :: Status
statusMonZombiePupaDecoy :: Status
statusMonZombiePupa :: Status
statusMonBrutePupaDecoy :: Status
statusMonBrutePupa :: Status
statusMonHulkPupaDecoy :: Status
statusMonHulkPupa :: Status
statusMonZombiePupaDecoyShady :: Status
statusMonZombiePupaShady :: Status
statusMonZombieWinged :: Status
statusMonZombieBruteWinged :: Status
statusMonSpawnRaptor :: Status
statusMonSpawnRaptorShady :: Status
statusMonSpawnRaptorUnstable :: Status
statusMonSpawnRaptorElectric :: Status
statusMonZombieAcidic :: Status
statusMonZombieCorrosive :: Status
statusMonZombieSpitter :: Status
statusMonZombieWretched :: Status
statusMonZombieDogAcidic :: Status
statusMonZombieDogBruteAcidic :: Status
statusMonMeatCocoonTiny :: Status
statusMonMeatCocoonSmall :: Status
statusMonMeatCocoonMed :: Status
statusMonMeatCocoonLarge :: Status
statusMonZombieChildScorched :: Status
statusMonZombieFiend :: Status
statusMonZombieScorched :: Status
statusMonZombieAnklebiter :: Status
statusMonZombieChild :: Status
statusMonZombieCreepy :: Status
statusMonZombieShriekling :: Status
statusMonZombieSnotgobbler :: Status
statusMonZombieSproglodyte :: Status
statusMonZombieWaif :: Status
statusMonZombieWretch :: Status
statusMonSkeletonMaster :: Status
statusMonSkeletonNecro :: Status
statusMonZombieBruteShocker :: Status
statusMonZombieElectric :: Status
statusMonZombieNullfield :: Status
statusMonZombieStatic :: Status
statusMonBoomer :: Status
statusMonBoomerHuge :: Status
statusMonGasZombie :: Status
statusMonZombieGasbag :: Status
statusMonZombieRelaxGasbag :: Status
statusMonZombieTearGasbag :: Status
statusMonBoomerGlutton :: Status
statusMonBoomerClaymore :: Status
statusMonZombieRust :: Status
statusMonZombieShell :: Status
statusMonZombiePlated :: Status
statusMonZombieUrchin :: Status
statusMonZombieHammerHands :: Status
statusMonCrawler :: Status
statusMonDevourer :: Status
statusMonDevourerLabSec :: Status
statusMonZombieCrushedGiant :: Status
statusMonZombieGasbagImmobile :: Status
statusMonZombieGasbagCrawler :: Status
statusMonZombieGasbagImpaler :: Status
statusMonZombieScissorlimbs :: Status
statusMonZombieHangingInnards :: Status
statusMonZombieGiantHeart :: Status
statusMonZombieLivingWall :: Status
statusMonZombieScientist :: Status
statusMonZombieLabsecurity :: Status
statusMonZombiePhaseSkulker :: Status
statusMonZombiePhaseShrike :: Status
statusMonFrogMother :: Status
statusMonTadpoleGrabber :: Status
statusMonZombullfrog :: Status
statusMonZombieBiter :: Status
statusMonZombieBrainless :: Status
statusMonAfsHeadlessHorror :: Status
statusMonZombieBrute :: Status
statusMonZombieBruteGrappler :: Status
statusMonZombieBruteNinja :: Status
statusMonZombieEars :: Status
statusMonZombieNemesis :: Status
statusMonZombieGrabber :: Status
statusMonZombieGrappler :: Status
statusMonZombieHollow :: Status
statusMonZombieHulk :: Status
statusMonZombieHunter :: Status
statusMonZombieJackson :: Status
statusMonZombieMancroc :: Status
statusMonZombieMaster :: Status
statusMonZombieNecro :: Status
statusMonZombieNecroBoomer :: Status
statusMonZombieRunner :: Status
statusMonZombieRegenerating :: Status
statusMonZombiePredator :: Status
statusMonZombieScreecher :: Status
statusMonZombieShady :: Status
statusMonShia :: Status
statusMonZombieShrieker :: Status
statusMonZombieSkull :: Status
statusMonZombieSmoker :: Status
statusMonSmokerBrute :: Status
statusMonZombieSwimmerBase :: Status
statusMonZombieSwimmer :: Status
statusMonZombieTechnician :: Status
statusMonZombieMiner :: Status
statusMonZombieThorny :: Status
statusMonZombieReenactor :: Status
statusMonZombiePrisoner :: Status
statusMonZombiePrisonerBrute :: Status
statusMonZombiePrisonerFat :: Status
statusMonZombiePrisonerTough :: Status
statusMonCharredNightmare :: Status
statusMonIrradiatedWanderer1 :: Status
statusMonIrradiatedWanderer2 :: Status
statusMonIrradiatedWanderer3 :: Status
statusMonIrradiatedWanderer4 :: Status
statusMonSkeleton :: Status
statusMonSkeletonBrute :: Status
statusMonSkeletonElectric :: Status
statusMonSkeletonHulk :: Status
statusMonZombieSoldier :: Status
statusMonZombieSoldierBlackops1 :: Status
statusMonZombieSoldierBlackops2 :: Status
statusMonZombieSoldierAcid1 :: Status
statusMonZombieSoldierAcid2 :: Status
statusMonZombieKevlar1 :: Status
statusMonZombieKevlar2 :: Status
statusMonZombieMilitaryPilot :: Status
statusMonZombieMilbasePersonnel :: Status
statusMonZombieSailor :: Status
statusMonZombieOfficer :: Status
statusMonZombieMarine :: Status
statusMonZombieMarineUpgrade :: Status
statusMonZombieArmored :: Status
statusMonZombieBioOp :: Status
statusMonZombieBioOp2 :: Status
statusMonZombieSurvivor :: Status
statusMonZombieSurvivorElite :: Status

statusMonFrogMother = emptyStatus
  & hp .~ 400
  & speed .~ 60
  & dodge .~ 2
  & armor.bash .~ 12
  & armor.bullet .~ 1
  & armor.cut .~ 2
  & armor.elec .~ 2
  & melee.skill .~ 5
  & melee.dice .~ 5
  & melee.diceSides .~ 6
  & regenerates .~ 40
  & melee.damage .~ [ emptyDamage
  & damageType .~ "bash"
  & amount .~ 5
  ]

statusMonTadpoleGrabber = emptyStatus
  & hp .~ 20
  & speed .~ 200
  & dodge .~ 4
  & melee.skill .~ 10
  & melee.dice .~ 1
  & melee.diceSides .~ 2

statusMonZombullfrog = emptyStatus
  & hp .~ 160
  & speed .~ 50
  & dodge .~ 1
  & armor.bash .~ 8
  & armor.bullet .~ 1
  & armor.cut .~ 2
  & armor.elec .~ 2
  & melee.skill .~ 4
  & melee.dice .~ 4
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "bash"
  & amount .~ 5
  ]

statusMonZombieBiter = emptyStatus
  & hp .~ 100
  & speed .~ 110
  & dodge .~ 3
  & armor.bash .~ 5
  & armor.bullet .~ 2
  & armor.cut .~ 2
  & armor.elec .~ 1
  & melee.skill .~ 6
  & melee.dice .~ 4
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 4
  ]

statusMonZombieBrainless = emptyStatus
  & hp .~ 60
  & speed .~ 65
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonAfsHeadlessHorror = emptyStatus
  & hp .~ 300
  & speed .~ 130
  & armor.elec .~ 2
  & melee.skill .~ 5
  & melee.dice .~ 4
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieBrute = emptyStatus
  & hp .~ 120
  & speed .~ 105
  & armor.bash .~ 4
  & armor.bullet .~ 5
  & armor.cut .~ 6
  & armor.elec .~ 2
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 8
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombieBruteGrappler = emptyStatus
  & hp .~ 280
  & speed .~ 95
  & dodge .~ 3
  & armor.bash .~ 10
  & armor.bullet .~ 8
  & armor.cut .~ 14
  & armor.stab .~ 8
  & armor.elec .~ 2
  & melee.skill .~ 7
  & melee.dice .~ 3
  & melee.diceSides .~ 8
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonZombieBruteNinja = emptyStatus
  & hp .~ 200
  & speed .~ 90
  & dodge .~ 4
  & armor.bash .~ 8
  & armor.bullet .~ 4
  & armor.cut .~ 5
  & armor.elec .~ 2
  & melee.skill .~ 5
  & melee.dice .~ 4
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 4
  ]

statusMonZombieEars = emptyStatus
  & hp .~ 90
  & speed .~ 70
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieNemesis = emptyStatus
  & hp .~ 480
  & speed .~ 95
  & armor.bash .~ 15
  & armor.bullet .~ 15
  & armor.cut .~ 15
  & armor.elec .~ 6
  & melee.skill .~ 5
  & melee.dice .~ 4
  & melee.diceSides .~ 8
  & regenerates .~ 15
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieGrabber = emptyStatus
  & hp .~ 95
  & speed .~ 80
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 5
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieGrappler = emptyStatus
  & hp .~ 110
  & speed .~ 85
  & dodge .~ 2
  & armor.elec .~ 1
  & melee.skill .~ 5
  & melee.dice .~ 4
  & melee.diceSides .~ 5
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieHollow = emptyStatus
  & hp .~ 100
  & speed .~ 70
  & dodge .~ 4
  & armor.bash .~ 8
  & armor.bullet .~ 10
  & armor.cut .~ 4
  & armor.stab .~ 10
  & armor.elec .~ 1
  & melee.skill .~ 6
  & melee.dice .~ 2
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 4
  ]

statusMonZombieHulk = emptyStatus
  & hp .~ 480
  & speed .~ 130
  & armor.bash .~ 8
  & armor.bullet .~ 10
  & armor.cut .~ 12
  & armor.elec .~ 5
  & melee.skill .~ 5
  & melee.dice .~ 4
  & melee.diceSides .~ 8
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieHunter = emptyStatus
  & hp .~ 90
  & speed .~ 110
  & dodge .~ 3
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombieJackson = emptyStatus
  & hp .~ 100
  & speed .~ 100
  & dodge .~ 6
  & armor.bash .~ 2
  & armor.bullet .~ 2
  & armor.cut .~ 2
  & armor.elec .~ 1
  & melee.skill .~ 9
  & melee.dice .~ 1
  & melee.diceSides .~ 5
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieMancroc = emptyStatus
  & hp .~ 120
  & speed .~ 100
  & dodge .~ 2
  & armor.bash .~ 8
  & armor.bullet .~ 8
  & armor.cut .~ 10
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 5
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 6
  ]

statusMonZombieMaster = emptyStatus
  & hp .~ 180
  & speed .~ 90
  & dodge .~ 4
  & armor.bash .~ 2
  & armor.bullet .~ 2
  & armor.cut .~ 2
  & armor.elec .~ 3
  & melee.skill .~ 5
  & melee.dice .~ 2
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieNecro = emptyStatus
  & hp .~ 100
  & speed .~ 100
  & dodge .~ 2
  & armor.elec .~ 2
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombieNecroBoomer = statusMonZombieNecro

statusMonZombieRunner = emptyStatus
  & hp .~ 70
  & speed .~ 105
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 4
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonZombieRegenerating = emptyStatus
  & hp .~ 60
  & speed .~ 90
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 3
  & regenerates .~ 12
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonZombiePredator = emptyStatus
  & hp .~ 90
  & speed .~ 140
  & dodge .~ 5
  & armor.bash .~ 5
  & armor.bullet .~ 4
  & armor.cut .~ 5
  & armor.elec .~ 1
  & melee.skill .~ 5
  & melee.dice .~ 4
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 5
  ]

statusMonZombieScreecher = emptyStatus
  & hp .~ 85
  & speed .~ 100
  & dodge .~ 2
  & armor.elec .~ 1
  & melee.skill .~ 5
  & melee.dice .~ 3
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 5
  ]

statusMonZombieShady = emptyStatus
  & hp .~ 80
  & speed .~ 70
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonShia = emptyStatus
  & hp .~ 50
  & speed .~ 110
  & dodge .~ 4
  & armor.elec .~ 1
  & melee.skill .~ 9
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 6
  ]

statusMonZombieShrieker = emptyStatus
  & hp .~ 60
  & speed .~ 95
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieSkull = emptyStatus
  & hp .~ 135
  & speed .~ 100
  & dodge .~ 1
  & armor.bash .~ 5
  & armor.bullet .~ 2
  & armor.cut .~ 3
  & armor.elec .~ 2
  & melee.skill .~ 4
  & melee.dice .~ 4
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombieSmoker = emptyStatus
  & hp .~ 75
  & speed .~ 110
  & dodge .~ 4
  & armor.heat .~ 5
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 1
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonSmokerBrute = emptyStatus
  & hp .~ 150
  & speed .~ 110
  & dodge .~ 7
  & armor.bash .~ 2
  & armor.bullet .~ 10
  & armor.cut .~ 8
  & armor.stab .~ 8
  & armor.heat .~ 5
  & armor.elec .~ 2
  & melee.skill .~ 5
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonZombieSwimmerBase = emptyStatus
  & hp .~ 60
  & speed .~ 90
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 2
  & melee.dice .~ 1
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 3
  ]

statusMonZombieSwimmer = emptyStatus
  & hp .~ 70
  & speed .~ 95
  & dodge .~ 2
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombieTechnician = emptyStatus
  & hp .~ 85
  & speed .~ 75
  & dodge .~ 1
  & armor.bash .~ 2
  & armor.bullet .~ 2
  & armor.cut .~ 2
  & armor.elec .~ 3
  & melee.skill .~ 5
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieMiner = emptyStatus
  & hp .~ 85
  & speed .~ 75
  & dodge .~ 1
  & armor.bash .~ 2
  & armor.bullet .~ 2
  & armor.cut .~ 2
  & armor.elec .~ 1
  & melee.skill .~ 5
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieThorny = emptyStatus
  & hp .~ 80
  & speed .~ 50
  & armor.bash .~ 2
  & armor.bullet .~ 3
  & armor.cut .~ 4
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 4
  ]

statusMonZombieReenactor = statusMonZombie

statusMonZombieAcidic = emptyStatus
  & hp .~ 65
  & speed .~ 80
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieCorrosive = emptyStatus
  & hp .~ 140
  & speed .~ 75
  & armor.bash .~ 10
  & armor.bullet .~ 2
  & armor.cut .~ 2
  & armor.stab .~ 12
  & armor.elec .~ 2
  & melee.skill .~ 1
  & melee.dice .~ 1
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieSpitter = emptyStatus
  & hp .~ 70
  & speed .~ 95
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieWretched = emptyStatus
  & hp .~ 55
  & speed .~ 65
  & armor.elec .~ 1
  & melee.skill .~ 2
  & melee.dice .~ 2
  & melee.diceSides .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonZombieDogAcidic = emptyStatus
  & hp .~ 24
  & speed .~ 85
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 1
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  , emptyDamage
  & damageType .~ "acid"
  & amount .~ 4
  ]

statusMonZombieDogBruteAcidic = statusMonZombieDogAcidic


statusMonSkeleton = emptyStatus
  & hp .~ 30
  & speed .~ 60
  & dodge .~ 2
  & armor.bullet .~ 24
  & armor.cut .~ 15
  & armor.stab .~ 30
  & armor.acid .~ 3
  & armor.elec .~ 3
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 4
  ]

statusMonSkeletonBrute = emptyStatus
  & hp .~ 175
  & speed .~ 60
  & dodge .~ 1
  & armor.bash .~ 12
  & armor.bullet .~ 30
  & armor.cut .~ 30
  & armor.acid .~ 1
  & armor.elec .~ 4
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 8
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 10
  ]

statusMonSkeletonElectric = emptyStatus
  & hp .~ 105
  & speed .~ 90
  & dodge .~ 2
  & armor.bullet .~ 30
  & armor.cut .~ 15
  & armor.stab .~ 30
  & armor.acid .~ 3
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 3
  ]

statusMonSkeletonHulk = emptyStatus
  & hp .~ 480
  & speed .~ 60
  & armor.bash .~ 20
  & armor.bullet .~ 36
  & armor.cut .~ 45
  & armor.elec .~ 8
  & melee.skill .~ 5
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 15
  ]


statusMonBeekeeper = emptyStatus
  & hp .~ 70
  & speed .~ 60
  & armor.bash .~ 6
  & armor.bullet .~ 6
  & armor.cut .~ 8
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombie = emptyStatus
  & hp .~ 80
  & speed .~ 70
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieCop = emptyStatus
  & hp .~ 80
  & speed .~ 70
  & armor.bash .~ 6
  & armor.bullet .~ 6
  & armor.cut .~ 6
  & armor.stab .~ 6
  & armor.elec .~ 2
  & melee.skill .~ 6
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieCrawler = emptyStatus
  & hp .~ 60
  & speed .~ 20
  & armor.elec .~ 1
  & melee.skill .~ 2
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieDancer = emptyStatus
  & hp .~ 10000
  & speed .~ 200
  & armor.bash .~ 12
  & armor.bullet .~ 6
  & armor.cut .~ 8
  & armor.elec .~ 5
  & melee.dice .~ 1
  & melee.diceSides .~ 5
  & regenerates .~ 50
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieFat = emptyStatus
  & hp .~ 95
  & speed .~ 55
  & armor.bash .~ 5
  & armor.bullet .~ 2
  & armor.cut .~ 3
  & armor.elec .~ 2
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieFireman = emptyStatus
  & hp .~ 80
  & speed .~ 70
  & armor.bash .~ 6
  & armor.bullet .~ 5
  & armor.cut .~ 6
  & armor.acid .~ 3
  & armor.heat .~ 10
  & armor.elec .~ 4
  & melee.skill .~ 5
  & melee.dice .~ 3
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieHazmat = emptyStatus
  & hp .~ 80
  & speed .~ 70
  & armor.bash .~ 5
  & armor.bullet .~ 3
  & armor.cut .~ 4
  & armor.elec .~ 5
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieRot = emptyStatus
  & hp .~ 55
  & speed .~ 70
  & armor.elec .~ 1
  & melee.skill .~ 1
  & melee.dice .~ 2
  & melee.diceSides .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieSwat = emptyStatus
  & hp .~ 90
  & speed .~ 75
  & armor.bash .~ 10
  & armor.bullet .~ 18
  & armor.cut .~ 16
  & armor.elec .~ 3
  & melee.skill .~ 6
  & melee.dice .~ 2
  & melee.diceSides .~ 5
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieTough = emptyStatus
  & hp .~ 100
  & speed .~ 85
  & dodge .~ 1
  & armor.bash .~ 2
  & armor.bullet .~ 1
  & armor.cut .~ 1
  & armor.elec .~ 2
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonZombieResortDancer = statusMonZombie
  & dodge .~ 1

statusMonZombieResortBouncer = statusMonZombieTough

statusMonZombieResortStaff = statusMonZombie


statusMonZombieWinged = emptyStatus
  & hp .~ 64
  & speed .~ 70
  & armor.elec .~ 1
  & melee.skill .~ 2
  & melee.dice .~ 2
  & melee.diceSides .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieBruteWinged = emptyStatus
  & hp .~ 70
  & speed .~ 65
  & armor.bash .~ 2
  & armor.bullet .~ 2
  & armor.cut .~ 2
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 3
  ]

statusMonSpawnRaptor = emptyStatus
  & hp .~ 20
  & speed .~ 140
  & dodge .~ 3
  & melee.skill .~ 9
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 4
  ]

statusMonSpawnRaptorShady = statusMonSpawnRaptor

statusMonSpawnRaptorUnstable = statusMonSpawnRaptor
  & speed .~ 100

statusMonSpawnRaptorElectric = statusMonSpawnRaptor
  & melee.damage .~ [ emptyDamage
  & damageType .~ "electric"
  & amount .~ 8
  ]


statusMonCrawler = emptyStatus
  & hp .~ 180
  & speed .~ 80
  & dodge .~ 1
  & armor.bash .~ 8
  & armor.elec .~ 2
  & melee.skill .~ 9
  & melee.dice .~ 2
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonDevourer = emptyStatus
  & hp .~ 112
  & speed .~ 60
  & armor.bash .~ 5
  & armor.bullet .~ 4
  & armor.cut .~ 5
  & armor.elec .~ 3
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 3
  & regenerates .~ 1
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonDevourerLabSec = statusMonDevourer
  & hp .~ 550
  & speed .~ 110
  & armor.bash .~ 26
  & armor.bullet .~ 40
  & armor.cut .~ 35
  & armor.stab .~ 30
  & armor.heat .~ 80
  & armor.elec .~ 6
  & melee.skill .~ 9
  & melee.dice .~ 9
  & melee.diceSides .~ 5
  & regenerates .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 5
  ]

statusMonZombieCrushedGiant = emptyStatus
  & hp .~ 500
  & speed .~ 100
  & armor.bash .~ 10
  & armor.bullet .~ 16
  & armor.cut .~ 20
  & armor.elec .~ 5
  & melee.skill .~ 7
  & melee.dice .~ 6
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 10
  ]

statusMonZombieGasbagImmobile = emptyStatus
  & hp .~ 5
  & speed .~ 80
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieGasbagCrawler = emptyStatus
  & hp .~ 15
  & speed .~ 90
  & armor.bash .~ 6
  & armor.bullet .~ 5
  & armor.cut .~ 6
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombieGasbagImpaler = emptyStatus
  & hp .~ 30
  & speed .~ 12
  & armor.bash .~ 6
  & armor.bullet .~ 5
  & armor.cut .~ 6
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieScissorlimbs = emptyStatus
  & hp .~ 10
  & speed .~ 250
  & armor.bash .~ 2
  & armor.bullet .~ 10
  & armor.cut .~ 10
  & armor.elec .~ 2
  & melee.skill .~ 5
  & melee.dice .~ 5
  & melee.diceSides .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 12
  ]

statusMonZombieHangingInnards = emptyStatus
  & hp .~ 200
  & speed .~ 110
  & armor.bash .~ 6
  & armor.bullet .~ 6
  & armor.cut .~ 10
  & armor.elec .~ 2
  & melee.skill .~ 5
  & melee.dice .~ 5
  & melee.diceSides .~ 2

statusMonZombieGiantHeart = emptyStatus
  & hp .~ 3
  & speed .~ 100
  & armor.bash .~ 25
  & armor.bullet .~ 20
  & armor.cut .~ 25
  & armor.elec .~ 5

statusMonZombieLivingWall = emptyStatus
  & hp .~ 3
  & speed .~ 100
  & armor.bash .~ 15
  & armor.bullet .~ 10
  & armor.cut .~ 15
  & armor.elec .~ 6


statusMonZombieSoldier = emptyStatus
  & hp .~ 100
  & speed .~ 80
  & dodge .~ 1
  & armor.bash .~ 12
  & armor.bullet .~ 20
  & armor.cut .~ 25
  & armor.elec .~ 3
  & melee.skill .~ 5
  & melee.dice .~ 2
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieSoldierBlackops1 = emptyStatus
  & hp .~ 120
  & speed .~ 90
  & dodge .~ 3
  & armor.bash .~ 12
  & armor.bullet .~ 20
  & armor.cut .~ 25
  & armor.elec .~ 3
  & melee.skill .~ 6
  & melee.dice .~ 2
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieSoldierBlackops2 = statusMonZombieSoldierBlackops1

statusMonZombieSoldierAcid1 = statusMonZombieSoldier

statusMonZombieSoldierAcid2 = statusMonZombieSoldier

statusMonZombieKevlar1 = emptyStatus
  & hp .~ 150
  & speed .~ 80
  & dodge .~ 1
  & armor.bash .~ 20
  & armor.bullet .~ 24
  & armor.cut .~ 30
  & armor.elec .~ 3
  & melee.skill .~ 6
  & melee.dice .~ 2
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieKevlar2 = emptyStatus
  & hp .~ 300
  & speed .~ 75
  & dodge .~ 1
  & armor.bash .~ 30
  & armor.bullet .~ 40
  & armor.cut .~ 50
  & armor.elec .~ 6
  & melee.skill .~ 8
  & melee.dice .~ 3
  & melee.diceSides .~ 8
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieMilitaryPilot = emptyStatus
  & hp .~ 90
  & speed .~ 85
  & dodge .~ 1
  & armor.bash .~ 5
  & armor.bullet .~ 4
  & armor.cut .~ 5
  & armor.elec .~ 2
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieMilbasePersonnel = statusMonZombieMilitaryPilot

statusMonZombieSailor = statusMonZombieMilbasePersonnel
  & armor.bash .~ 2
  & armor.bullet .~ 2
  & armor.cut .~ 2

statusMonZombieOfficer = statusMonZombieSailor

statusMonZombieMarine = emptyStatus
  & hp .~ 100
  & speed .~ 80
  & dodge .~ 1
  & armor.bash .~ 12
  & armor.bullet .~ 20
  & armor.cut .~ 25
  & armor.elec .~ 3
  & melee.skill .~ 6
  & melee.dice .~ 3
  & melee.diceSides .~ 4

statusMonZombieMarineUpgrade = statusMonZombieMarine
  & hp .~ 150
  & melee.diceSides .~ 6

statusMonZombieArmored = emptyStatus
  & hp .~ 120
  & speed .~ 70
  & armor.bash .~ 64
  & armor.bullet .~ 51
  & armor.cut .~ 64
  & armor.acid .~ 20
  & armor.heat .~ 20
  & armor.elec .~ 10
  & melee.skill .~ 5
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieBioOp = emptyStatus
  & hp .~ 120
  & speed .~ 95
  & dodge .~ 3
  & armor.bash .~ 10
  & armor.bullet .~ 14
  & armor.cut .~ 18
  & armor.elec .~ 3
  & melee.skill .~ 7
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "electric"
  & amount .~ 4
  ]

statusMonZombieBioOp2 = statusMonZombieBioOp
  & hp .~ 180
  & dodge .~ 4
  & armor.bash .~ 15
  & armor.cut .~ 27
  & melee.skill .~ 8


statusMonZombieCrawlerPupaDecoy = statusMonZombieCrawler
  & armor.bash .~ 7
  & armor.bullet .~ 5
  & armor.cut .~ 5
  & regenerates .~ 10

statusMonZombieCrawlerPupa = statusMonZombieCrawlerPupaDecoy

statusMonZombiePupaDecoy = statusMonZombieFat
  & armor.bash .~ 8
  & armor.bullet .~ 6
  & armor.cut .~ 6
  & regenerates .~ 10

statusMonZombiePupa = statusMonZombiePupaDecoy

statusMonBrutePupaDecoy = statusMonZombieBrute
  & speed .~ 95
  & regenerates .~ 10

statusMonBrutePupa = statusMonBrutePupaDecoy

statusMonHulkPupaDecoy = emptyStatus
  & hp .~ 300
  & speed .~ 85
  & armor.bash .~ 8
  & armor.bullet .~ 10
  & armor.cut .~ 10
  & armor.elec .~ 6
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 7
  & regenerates .~ 10
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonHulkPupa = emptyStatus
  & hp .~ 300
  & speed .~ 85
  & armor.bash .~ 8
  & armor.bullet .~ 10
  & armor.cut .~ 10
  & armor.elec .~ 6
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 7
  & regenerates .~ 10
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombiePupaDecoyShady = statusMonZombieFat
  & armor.bash .~ 8
  & armor.bullet .~ 6
  & armor.cut .~ 6
  & regenerates .~ 5

statusMonZombiePupaShady = statusMonZombiePupaDecoy


statusMonSkeletonMaster = emptyStatus
  & hp .~ 90
  & speed .~ 60
  & dodge .~ 2
  & armor.bullet .~ 24
  & armor.cut .~ 15
  & armor.stab .~ 30
  & armor.acid .~ 3
  & armor.elec .~ 3
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 4
  ]

statusMonSkeletonNecro = emptyStatus
  & hp .~ 100
  & speed .~ 70
  & dodge .~ 2
  & armor.bullet .~ 24
  & armor.cut .~ 15
  & armor.stab .~ 30
  & armor.acid .~ 3
  & armor.elec .~ 3
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 4
  ]


statusMonZombieChildScorched = emptyStatus
  & hp .~ 35
  & speed .~ 90
  & armor.bash .~ 4
  & armor.bullet .~ 4
  & armor.cut .~ 5
  & armor.acid .~ 3
  & armor.heat .~ 15
  & armor.elec .~ 3
  & melee.skill .~ 2
  & melee.dice .~ 1
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 5
  ]

statusMonZombieFiend = emptyStatus
  & hp .~ 100
  & speed .~ 100
  & armor.bash .~ 6
  & armor.bullet .~ 10
  & armor.cut .~ 12
  & armor.acid .~ 5
  & armor.heat .~ 15
  & armor.elec .~ 3
  & melee.skill .~ 4
  & melee.dice .~ 1
  & melee.diceSides .~ 10
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 14
  ]

statusMonZombieScorched = emptyStatus
  & hp .~ 40
  & speed .~ 60
  & armor.bash .~ 2
  & armor.bullet .~ 7
  & armor.cut .~ 9
  & armor.acid .~ 3
  & armor.heat .~ 15
  & armor.elec .~ 3
  & melee.skill .~ 2
  & melee.dice .~ 2
  & melee.diceSides .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]


statusMonZombieBruteShocker = emptyStatus
  & hp .~ 275
  & speed .~ 110
  & armor.bash .~ 3
  & armor.bullet .~ 6
  & armor.cut .~ 8
  & melee.skill .~ 3
  & melee.dice .~ 3
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "electric"
  & amount .~ 4
  , emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombieElectric = emptyStatus
  & hp .~ 85
  & speed .~ 105
  & dodge .~ 2
  & melee.skill .~ 4
  & melee.dice .~ 1
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "electric"
  & amount .~ 8
  ]

statusMonZombieNullfield = emptyStatus
  & hp .~ 200
  & speed .~ 50
  & melee.skill .~ 5
  & melee.dice .~ 1
  & melee.diceSides .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "electric"
  & amount .~ 6
  ]

statusMonZombieStatic = emptyStatus
  & hp .~ 60
  & speed .~ 75
  & dodge .~ 2
  & melee.skill .~ 4
  & melee.dice .~ 1
  & melee.diceSides .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "electric"
  & amount .~ 4
  ]


statusMonZombieRust = emptyStatus
  & hp .~ 80
  & speed .~ 65
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieShell = emptyStatus
  & hp .~ 200
  & speed .~ 50
  & armor.bash .~ 8
  & armor.bullet .~ 8
  & armor.cut .~ 8
  & armor.stab .~ 8
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombiePlated = emptyStatus
  & hp .~ 170
  & speed .~ 60
  & armor.bash .~ 15
  & armor.bullet .~ 15
  & armor.cut .~ 15
  & armor.stab .~ 15
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieUrchin = emptyStatus
  & hp .~ 100
  & speed .~ 60
  & dodge .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 12
  ]

statusMonZombieHammerHands = emptyStatus
  & hp .~ 100
  & speed .~ 100
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 8
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]


statusMonZombieMedical = statusMonZombie

statusMonZombieMedicalBrute = statusMonZombieBrute

statusMonZombieMedicalRegenerating = statusMonZombieRegenerating

statusMonSkeletonMedical = statusMonSkeleton

statusMonZombieMedicalAcidic = statusMonZombieAcidic

statusMonZombieMedicalPupa = statusMonZombiePupa

statusMonZombiePupaMedicalDecoy = statusMonZombiePupaDecoy


statusMonZombieScientist = emptyStatus
  & hp .~ 80
  & speed .~ 75
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombieLabsecurity = emptyStatus
  & hp .~ 100
  & speed .~ 85
  & armor.bash .~ 4
  & armor.bullet .~ 4
  & armor.cut .~ 6
  & armor.stab .~ 4
  & armor.elec .~ 2
  & melee.skill .~ 5
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombiePhaseSkulker = emptyStatus
  & hp .~ 80
  & speed .~ 90
  & dodge .~ 5
  & armor.bash .~ 5
  & armor.bullet .~ 4
  & armor.cut .~ 5
  & armor.elec .~ 3
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 5
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonZombiePhaseShrike = statusMonZombiePhaseSkulker
  & hp .~ 10
  & dodge .~ 10
  & armor.bash .~ 40
  & armor.bullet .~ 30
  & armor.cut .~ 40
  & melee.skill .~ 6
  & melee.diceSides .~ 9
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]


statusMonZombieAnklebiter = emptyStatus
  & hp .~ 35
  & speed .~ 105
  & dodge .~ 3
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombieChild = emptyStatus
  & hp .~ 40
  & speed .~ 65
  & dodge .~ 2
  & armor.elec .~ 1
  & melee.skill .~ 2
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombieCreepy = emptyStatus
  & hp .~ 25
  & speed .~ 55
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 2
  & melee.dice .~ 2
  & melee.diceSides .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonZombieShriekling = emptyStatus
  & hp .~ 20
  & speed .~ 95
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 2
  & melee.dice .~ 2
  & melee.diceSides .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonZombieSnotgobbler = emptyStatus
  & hp .~ 25
  & speed .~ 65
  & dodge .~ 1
  & armor.bash .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZombieSproglodyte = emptyStatus
  & hp .~ 30
  & speed .~ 85
  & dodge .~ 2
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 3
  ]

statusMonZombieWaif = emptyStatus
  & hp .~ 15
  & speed .~ 65
  & dodge .~ 2
  & armor.bullet .~ 4
  & armor.cut .~ 5
  & armor.elec .~ 1
  & melee.skill .~ 2
  & melee.dice .~ 2
  & melee.diceSides .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonZombieWretch = emptyStatus
  & hp .~ 20
  & speed .~ 20
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 2
  & melee.dice .~ 2
  & melee.diceSides .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]


statusMonZombiePrisoner = statusMonZombie
  & hp .~ 90
  & melee.diceSides .~ 4

statusMonZombiePrisonerBrute = statusMonZombieBrute

statusMonZombiePrisonerFat = statusMonZombieFat

statusMonZombiePrisonerTough = statusMonZombieTough


statusMonZombieSurvivor = emptyStatus
  & hp .~ 120
  & speed .~ 90
  & dodge .~ 3
  & armor.bash .~ 7
  & armor.bullet .~ 6
  & armor.cut .~ 7
  & armor.acid .~ 4
  & armor.elec .~ 2
  & melee.skill .~ 6
  & melee.dice .~ 3
  & melee.diceSides .~ 3

statusMonZombieSurvivorElite = statusMonZombieSurvivor
  & speed .~ 110
  & armor.bash .~ 15
  & armor.bullet .~ 20
  & armor.cut .~ 25
  & armor.stab .~ 18
  & armor.elec .~ 4
  & melee.skill .~ 9
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 5
  ]


statusMonZhark = emptyStatus
  & hp .~ 250
  & speed .~ 200
  & dodge .~ 5
  & armor.bash .~ 6
  & armor.bullet .~ 2
  & armor.cut .~ 2
  & melee.skill .~ 7
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 8
  ]

statusMonZombieDog = emptyStatus
  & hp .~ 36
  & speed .~ 105
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 1
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonDogZombieCop = emptyStatus
  & hp .~ 42
  & speed .~ 105
  & dodge .~ 1
  & armor.bash .~ 6
  & armor.bullet .~ 5
  & armor.cut .~ 6
  & armor.elec .~ 1
  & melee.skill .~ 5
  & melee.dice .~ 2
  & melee.diceSides .~ 5
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonDogZombieRot = emptyStatus
  & hp .~ 24
  & speed .~ 105
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 1
  & melee.dice .~ 1
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZolf = emptyStatus
  & hp .~ 48
  & speed .~ 116
  & dodge .~ 2
  & armor.elec .~ 1
  & melee.skill .~ 5
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 4
  ]

statusMonZombear = emptyStatus
  & hp .~ 180
  & speed .~ 120
  & dodge .~ 2
  & armor.bash .~ 2
  & armor.bullet .~ 2
  & armor.cut .~ 2
  & armor.elec .~ 2
  & melee.skill .~ 6
  & melee.dice .~ 4
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 6
  ]

statusMonZombiePig = emptyStatus
  & hp .~ 55
  & speed .~ 70
  & dodge .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 5
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 4
  ]

statusMonZombeaver = emptyStatus
  & hp .~ 30
  & speed .~ 80
  & dodge .~ 2
  & armor.elec .~ 1
  & melee.skill .~ 1
  & melee.dice .~ 1
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 6
  ]

statusMonZoose = emptyStatus
  & hp .~ 210
  & speed .~ 140
  & armor.bash .~ 6
  & armor.bullet .~ 3
  & armor.cut .~ 4
  & armor.elec .~ 2
  & melee.skill .~ 6
  & melee.dice .~ 3
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZougar = emptyStatus
  & hp .~ 65
  & speed .~ 126
  & dodge .~ 2
  & armor.bash .~ 1
  & armor.elec .~ 1
  & melee.skill .~ 6
  & melee.dice .~ 2
  & melee.diceSides .~ 5
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 6
  ]

statusMonZombieHorse = emptyStatus
  & hp .~ 70
  & speed .~ 200
  & armor.bash .~ 5
  & armor.cut .~ 1
  & armor.elec .~ 2
  & melee.skill .~ 4
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 6
  ]

statusMonZiger = statusMonZougar
  & hp .~ 200
  & speed .~ 110
  & dodge .~ 0
  & armor.elec .~ 2
  & melee.dice .~ 3

statusMonZow = emptyStatus
  & hp .~ 166
  & speed .~ 104
  & armor.bash .~ 3
  & armor.cut .~ 3
  & armor.elec .~ 3
  & melee.skill .~ 6
  & melee.dice .~ 2
  & melee.diceSides .~ 10
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 6
  ]

statusMonZombull = emptyStatus
  & hp .~ 210
  & speed .~ 120
  & armor.bash .~ 6
  & armor.bullet .~ 3
  & armor.cut .~ 4
  & armor.elec .~ 4
  & melee.skill .~ 6
  & melee.dice .~ 4
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 6
  ]

statusMonZllama = emptyStatus
  & hp .~ 90
  & speed .~ 90
  & dodge .~ 2
  & melee.skill .~ 3
  & melee.dice .~ 1
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZeer = emptyStatus
  & hp .~ 100
  & speed .~ 200
  & armor.bash .~ 1
  & armor.cut .~ 3
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 3
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZpiderMass = emptyStatus
  & hp .~ 40
  & speed .~ 70
  & dodge .~ 1
  & armor.bullet .~ 2
  & armor.cut .~ 3
  & armor.stab .~ 3
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 2
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonZeindeer = emptyStatus
  & hp .~ 160
  & speed .~ 200
  & armor.bash .~ 5
  & armor.cut .~ 4
  & armor.elec .~ 1
  & melee.skill .~ 4
  & melee.dice .~ 4
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonGastroBufo = emptyStatus
  & hp .~ 200
  & speed .~ 75
  & dodge .~ 2
  & armor.bash .~ 8
  & armor.bullet .~ 1
  & armor.cut .~ 2
  & melee.skill .~ 5
  & melee.dice .~ 5
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "bash"
  & amount .~ 5
  ]


statusMonCharredNightmare = emptyStatus
  & hp .~ 120
  & speed .~ 95
  & dodge .~ 2
  & armor.bash .~ 6
  & armor.bullet .~ 5
  & armor.cut .~ 6
  & armor.elec .~ 3
  & melee.skill .~ 5
  & melee.dice .~ 3
  & melee.diceSides .~ 10
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonIrradiatedWanderer1 = emptyStatus
  & hp .~ 90
  & speed .~ 80
  & dodge .~ 1
  & armor.bash .~ 4
  & armor.bullet .~ 3
  & armor.cut .~ 4
  & armor.elec .~ 2
  & melee.skill .~ 6
  & melee.dice .~ 2
  & melee.diceSides .~ 7
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonIrradiatedWanderer2 = emptyStatus
  & hp .~ 90
  & speed .~ 88
  & dodge .~ 1
  & armor.bash .~ 5
  & armor.bullet .~ 4
  & armor.cut .~ 5
  & armor.elec .~ 2
  & melee.skill .~ 6
  & melee.dice .~ 2
  & melee.diceSides .~ 8
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonIrradiatedWanderer3 = emptyStatus
  & hp .~ 100
  & speed .~ 95
  & dodge .~ 1
  & armor.bash .~ 6
  & armor.bullet .~ 5
  & armor.cut .~ 6
  & armor.elec .~ 2
  & melee.skill .~ 7
  & melee.dice .~ 3
  & melee.diceSides .~ 6
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]

statusMonIrradiatedWanderer4 = emptyStatus
  & hp .~ 120
  & speed .~ 100
  & dodge .~ 2
  & armor.bash .~ 8
  & armor.bullet .~ 6
  & armor.cut .~ 8
  & armor.elec .~ 2
  & melee.skill .~ 8
  & melee.dice .~ 2
  & melee.diceSides .~ 10
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 1
  ]


statusMonBoomer = emptyStatus
  & hp .~ 40
  & speed .~ 55
  & armor.elec .~ 2
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonBoomerHuge = emptyStatus
  & hp .~ 110
  & speed .~ 85
  & armor.bash .~ 5
  & armor.bullet .~ 4
  & armor.cut .~ 5
  & armor.elec .~ 3
  & melee.skill .~ 1
  & melee.dice .~ 3
  & melee.diceSides .~ 3
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonGasZombie = emptyStatus
  & hp .~ 90
  & speed .~ 25
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieGasbag = emptyStatus
  & hp .~ 15
  & speed .~ 80
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieRelaxGasbag = emptyStatus
  & hp .~ 40
  & speed .~ 70
  & armor.elec .~ 1
  & melee.skill .~ 3
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonZombieTearGasbag = emptyStatus
  & hp .~ 45
  & speed .~ 60
  & armor.elec .~ 1
  & melee.skill .~ 2
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 0
  ]

statusMonBoomerGlutton = emptyStatus
  & hp .~ 200
  & speed .~ 75
  & armor.bash .~ 20
  & armor.bullet .~ 5
  & armor.cut .~ 5
  & armor.elec .~ 2
  & melee.skill .~ 4

statusMonBoomerClaymore = emptyStatus
  & hp .~ 150
  & speed .~ 95
  & armor.bash .~ 10
  & armor.bullet .~ 5
  & armor.cut .~ 5
  & armor.elec .~ 2
  & melee.skill .~ 1


statusMonMeatCocoonTiny = statusMonMeatCocoonSmall

statusMonMeatCocoonSmall = emptyStatus
  & hp .~ 100
  & speed .~ 100

statusMonMeatCocoonMed = statusMonMeatCocoonSmall
  & hp .~ 200

statusMonMeatCocoonLarge = statusMonMeatCocoonSmall
  & hp .~ 600

statusMonAmalgamationAbstractSmall :: Status
statusMonAmalgamationAbstractMed :: Status
statusMonAmalgamationSwarmer :: Status
statusMonAmalgamationSpotter :: Status
statusMonAmalgamationCorroder :: Status
statusMonAmalgamationJumper :: Status
statusMonAmalgamationZapper :: Status
statusMonAmalgamationSoldier :: Status

statusMonAmalgamationAbstractSmall = emptyStatus
  & hp .~ 160
  & speed .~ 110
  & dodge .~ 6
  & armor.bash .~ 8
  & armor.bullet .~ 12
  & armor.cut .~ 4
  & armor.stab .~ 4
  & armor.acid .~ 5
  & armor.elec .~ 5
  & melee.skill .~ 5
  & melee.dice .~ 3
  & melee.diceSides .~ 3

statusMonAmalgamationAbstractMed = statusMonAmalgamationAbstractSmall
  & hp .~ 180
  & dodge .~ 5
  & armor.bash .~ 16
  & armor.bullet .~ 30
  & armor.cut .~ 10
  & armor.stab .~ 16
  & armor.acid .~ 8
  & armor.elec .~ 8
  & melee.skill .~ 8

statusMonAmalgamationSwarmer = statusMonAmalgamationAbstractSmall
  & hp .~ 35
  & speed .~ 120
  & melee.dice .~ 2
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 3
  ]

statusMonAmalgamationSpotter = statusMonAmalgamationAbstractSmall
  & armor.bash .~ 10
  & armor.cut .~ 5
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 2
  ]

statusMonAmalgamationCorroder = statusMonAmalgamationAbstractSmall
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 5
  , emptyDamage
  & damageType .~ "acid"
  & amount .~ 3
  ]

statusMonAmalgamationJumper = statusMonAmalgamationAbstractMed
  & hp .~ 135
  & speed .~ 100
  & dodge .~ 7
  & melee.dice .~ 2
  & melee.diceSides .~ 4
  & melee.damage .~ [ emptyDamage
  & damageType .~ "cut"
  & amount .~ 5
  & armorPenetration .~ 5
  ]

statusMonAmalgamationZapper = statusMonAmalgamationAbstractMed
  & speed .~ 95
  & melee.damage .~ [ emptyDamage
  & damageType .~ "electric"
  & amount .~ 5
  ]

statusMonAmalgamationSoldier = statusMonAmalgamationAbstractMed
  & speed .~ 100
  & dodge .~ 3
  & armor.bash .~ 20
  & armor.bullet .~ 40
  & armor.cut .~ 25
  & armor.stab .~ 30
  & armor.acid .~ 12
  & armor.elec .~ 12
  & melee.skill .~ 10
  & melee.dice .~ 4
  & melee.diceSides .~ 4


statusMonShoggoth :: Status
statusMonShoggoth = emptyStatus
  & hp .~ 400
  & speed .~ 90
  & melee.skill .~ 10
  & melee.dice .~ 7
  & melee.diceSides .~ 5
  & melee.damage .~ [ emptyDamage
      & damageType .~ "acid"
      & amount .~ 20
                    , emptyDamage
      & damageType .~ "cut"
      & amount .~ 6
  ]
  & armor.bash .~ 10
  & armor.cut .~ 30
  & armor.bullet .~ 24
  & armor.elec .~ 18

monsterStatus :: M.Map Id Status
monsterStatus = M.fromList
  [ (,) monZhark                     statusMonZhark
  , (,) monZombieDog                 statusMonZombieDog
  , (,) monDogZombieCop              statusMonDogZombieCop
  , (,) monDogZombieRot              statusMonDogZombieRot
  , (,) monZolf                      statusMonZolf
  , (,) monZombear                   statusMonZombear
  , (,) monZombiePig                 statusMonZombiePig
  , (,) monZombeaver                 statusMonZombeaver
  , (,) monZoose                     statusMonZoose
  , (,) monZougar                    statusMonZougar
  , (,) monZombieHorse               statusMonZombieHorse
  , (,) monZiger                     statusMonZiger
  , (,) monZow                       statusMonZow
  , (,) monZombull                   statusMonZombull
  , (,) monZllama                    statusMonZllama
  , (,) monZeer                      statusMonZeer
  , (,) monZpiderMass                statusMonZpiderMass
  , (,) monZeindeer                  statusMonZeindeer
  , (,) monGastroBufo                statusMonGastroBufo
  , (,) monBeekeeper                 statusMonBeekeeper
  , (,) monZombie                    statusMonZombie
  , (,) monZombieCop                 statusMonZombieCop
  , (,) monZombieCrawler             statusMonZombieCrawler
  , (,) monZombieDancer              statusMonZombieDancer
  , (,) monZombieFat                 statusMonZombieFat
  , (,) monZombieFireman             statusMonZombieFireman
  , (,) monZombieHazmat              statusMonZombieHazmat
  , (,) monZombieRot                 statusMonZombieRot
  , (,) monZombieSwat                statusMonZombieSwat
  , (,) monZombieTough               statusMonZombieTough
  , (,) monZombieResortDancer        statusMonZombieResortDancer
  , (,) monZombieResortBouncer       statusMonZombieResortBouncer
  , (,) monZombieResortStaff         statusMonZombieResortStaff
  , (,) monZombieMedical             statusMonZombieMedical
  , (,) monZombieMedicalBrute        statusMonZombieMedicalBrute
  , (,) monZombieMedicalRegenerating statusMonZombieMedicalRegenerating
  , (,) monSkeletonMedical           statusMonSkeletonMedical
  , (,) monZombieMedicalAcidic       statusMonZombieMedicalAcidic
  , (,) monZombieMedicalPupa         statusMonZombieMedicalPupa
  , (,) monZombiePupaMedicalDecoy    statusMonZombiePupaMedicalDecoy
  , (,) monZombieCrawlerPupaDecoy    statusMonZombieCrawlerPupaDecoy
  , (,) monZombieCrawlerPupa         statusMonZombieCrawlerPupa
  , (,) monZombiePupaDecoy           statusMonZombiePupaDecoy
  , (,) monZombiePupa                statusMonZombiePupa
  , (,) monBrutePupaDecoy            statusMonBrutePupaDecoy
  , (,) monBrutePupa                 statusMonBrutePupa
  , (,) monHulkPupaDecoy             statusMonHulkPupaDecoy
  , (,) monHulkPupa                  statusMonHulkPupa
  , (,) monZombiePupaDecoyShady      statusMonZombiePupaDecoyShady
  , (,) monZombiePupaShady           statusMonZombiePupaShady
  , (,) monZombieWinged              statusMonZombieWinged
  , (,) monZombieBruteWinged         statusMonZombieBruteWinged
  , (,) monSpawnRaptor               statusMonSpawnRaptor
  , (,) monSpawnRaptorShady          statusMonSpawnRaptorShady
  , (,) monSpawnRaptorUnstable       statusMonSpawnRaptorUnstable
  , (,) monSpawnRaptorElectric       statusMonSpawnRaptorElectric
  , (,) monZombieAcidic              statusMonZombieAcidic
  , (,) monZombieCorrosive           statusMonZombieCorrosive
  , (,) monZombieSpitter             statusMonZombieSpitter
  , (,) monZombieWretched            statusMonZombieWretched
  , (,) monZombieDogAcidic           statusMonZombieDogAcidic
  , (,) monZombieDogBruteAcidic      statusMonZombieDogBruteAcidic
  , (,) monMeatCocoonTiny            statusMonMeatCocoonTiny
  , (,) monMeatCocoonSmall           statusMonMeatCocoonSmall
  , (,) monMeatCocoonMed             statusMonMeatCocoonMed
  , (,) monMeatCocoonLarge           statusMonMeatCocoonLarge
  , (,) monZombieChildScorched       statusMonZombieChildScorched
  , (,) monZombieFiend               statusMonZombieFiend
  , (,) monZombieScorched            statusMonZombieScorched
  , (,) monZombieAnklebiter          statusMonZombieAnklebiter
  , (,) monZombieChild               statusMonZombieChild
  , (,) monZombieCreepy              statusMonZombieCreepy
  , (,) monZombieShriekling          statusMonZombieShriekling
  , (,) monZombieSnotgobbler         statusMonZombieSnotgobbler
  , (,) monZombieSproglodyte         statusMonZombieSproglodyte
  , (,) monZombieWaif                statusMonZombieWaif
  , (,) monZombieWretch              statusMonZombieWretch
  , (,) monSkeletonMaster            statusMonSkeletonMaster
  , (,) monSkeletonNecro             statusMonSkeletonNecro
  , (,) monZombieBruteShocker        statusMonZombieBruteShocker
  , (,) monZombieElectric            statusMonZombieElectric
  , (,) monZombieNullfield           statusMonZombieNullfield
  , (,) monZombieStatic              statusMonZombieStatic
  , (,) monBoomer                    statusMonBoomer
  , (,) monBoomerHuge                statusMonBoomerHuge
  , (,) monGasZombie                 statusMonGasZombie
  , (,) monZombieGasbag              statusMonZombieGasbag
  , (,) monZombieRelaxGasbag         statusMonZombieRelaxGasbag
  , (,) monZombieTearGasbag          statusMonZombieTearGasbag
  , (,) monBoomerGlutton             statusMonBoomerGlutton
  , (,) monBoomerClaymore            statusMonBoomerClaymore
  , (,) monZombieRust                statusMonZombieRust
  , (,) monZombieShell               statusMonZombieShell
  , (,) monZombiePlated              statusMonZombiePlated
  , (,) monZombieUrchin              statusMonZombieUrchin
  , (,) monZombieHammerHands         statusMonZombieHammerHands
  , (,) monCrawler                   statusMonCrawler
  , (,) monDevourer                  statusMonDevourer
  , (,) monDevourerLabSec            statusMonDevourerLabSec
  , (,) monZombieCrushedGiant        statusMonZombieCrushedGiant
  , (,) monZombieGasbagImmobile      statusMonZombieGasbagImmobile
  , (,) monZombieGasbagCrawler       statusMonZombieGasbagCrawler
  , (,) monZombieGasbagImpaler       statusMonZombieGasbagImpaler
  , (,) monZombieScissorlimbs        statusMonZombieScissorlimbs
  , (,) monZombieHangingInnards      statusMonZombieHangingInnards
  , (,) monZombieGiantHeart          statusMonZombieGiantHeart
  , (,) monZombieLivingWall          statusMonZombieLivingWall
  , (,) monZombieScientist           statusMonZombieScientist
  , (,) monZombieLabsecurity         statusMonZombieLabsecurity
  , (,) monZombiePhaseSkulker        statusMonZombiePhaseSkulker
  , (,) monZombiePhaseShrike         statusMonZombiePhaseShrike
  , (,) monFrogMother                statusMonFrogMother
  , (,) monTadpoleGrabber            statusMonTadpoleGrabber
  , (,) monZombullfrog               statusMonZombullfrog
  , (,) monZombieBiter               statusMonZombieBiter
  , (,) monZombieBrainless           statusMonZombieBrainless
  , (,) monAfsHeadlessHorror         statusMonAfsHeadlessHorror
  , (,) monZombieBrute               statusMonZombieBrute
  , (,) monZombieBruteGrappler       statusMonZombieBruteGrappler
  , (,) monZombieBruteNinja          statusMonZombieBruteNinja
  , (,) monZombieEars                statusMonZombieEars
  , (,) monZombieNemesis             statusMonZombieNemesis
  , (,) monZombieGrabber             statusMonZombieGrabber
  , (,) monZombieGrappler            statusMonZombieGrappler
  , (,) monZombieHollow              statusMonZombieHollow
  , (,) monZombieHulk                statusMonZombieHulk
  , (,) monZombieHunter              statusMonZombieHunter
  , (,) monZombieJackson             statusMonZombieJackson
  , (,) monZombieMancroc             statusMonZombieMancroc
  , (,) monZombieMaster              statusMonZombieMaster
  , (,) monZombieNecro               statusMonZombieNecro
  , (,) monZombieNecroBoomer         statusMonZombieNecroBoomer
  , (,) monZombieRunner              statusMonZombieRunner
  , (,) monZombieRegenerating        statusMonZombieRegenerating
  , (,) monZombiePredator            statusMonZombiePredator
  , (,) monZombieScreecher           statusMonZombieScreecher
  , (,) monZombieShady               statusMonZombieShady
  , (,) monShia                      statusMonShia
  , (,) monZombieShrieker            statusMonZombieShrieker
  , (,) monZombieSkull               statusMonZombieSkull
  , (,) monZombieSmoker              statusMonZombieSmoker
  , (,) monSmokerBrute               statusMonSmokerBrute
  , (,) monZombieSwimmerBase         statusMonZombieSwimmerBase
  , (,) monZombieSwimmer             statusMonZombieSwimmer
  , (,) monZombieTechnician          statusMonZombieTechnician
  , (,) monZombieMiner               statusMonZombieMiner
  , (,) monZombieThorny              statusMonZombieThorny
  , (,) monZombieReenactor           statusMonZombieReenactor
  , (,) monZombiePrisoner            statusMonZombiePrisoner
  , (,) monZombiePrisonerBrute       statusMonZombiePrisonerBrute
  , (,) monZombiePrisonerFat         statusMonZombiePrisonerFat
  , (,) monZombiePrisonerTough       statusMonZombiePrisonerTough
  , (,) monCharredNightmare          statusMonCharredNightmare
  , (,) monIrradiatedWanderer1       statusMonIrradiatedWanderer1
  , (,) monIrradiatedWanderer2       statusMonIrradiatedWanderer2
  , (,) monIrradiatedWanderer3       statusMonIrradiatedWanderer3
  , (,) monIrradiatedWanderer4       statusMonIrradiatedWanderer4
  , (,) monSkeleton                  statusMonSkeleton
  , (,) monSkeletonBrute             statusMonSkeletonBrute
  , (,) monSkeletonElectric          statusMonSkeletonElectric
  , (,) monSkeletonHulk              statusMonSkeletonHulk
  , (,) monZombieSoldier             statusMonZombieSoldier
  , (,) monZombieSoldierBlackops1    statusMonZombieSoldierBlackops1
  , (,) monZombieSoldierBlackops2    statusMonZombieSoldierBlackops2
  , (,) monZombieSoldierAcid1        statusMonZombieSoldierAcid1
  , (,) monZombieSoldierAcid2        statusMonZombieSoldierAcid2
  , (,) monZombieKevlar1             statusMonZombieKevlar1
  , (,) monZombieKevlar2             statusMonZombieKevlar2
  , (,) monZombieMilitaryPilot       statusMonZombieMilitaryPilot
  , (,) monZombieMilbasePersonnel    statusMonZombieMilbasePersonnel
  , (,) monZombieSailor              statusMonZombieSailor
  , (,) monZombieOfficer             statusMonZombieOfficer
  , (,) monZombieMarine              statusMonZombieMarine
  , (,) monZombieMarineUpgrade       statusMonZombieMarineUpgrade
  , (,) monZombieArmored             statusMonZombieArmored
  , (,) monZombieBioOp               statusMonZombieBioOp
  , (,) monZombieBioOp2              statusMonZombieBioOp2
  , (,) monZombieSurvivor            statusMonZombieSurvivor
  , (,) monZombieSurvivorElite       statusMonZombieSurvivorElite
  , (,) monAmalgamationSwarmer       statusMonAmalgamationSwarmer
  , (,) monAmalgamationSpotter       statusMonAmalgamationSpotter
  , (,) monAmalgamationCorroder      statusMonAmalgamationCorroder
  , (,) monAmalgamationJumper        statusMonAmalgamationJumper
  , (,) monAmalgamationZapper        statusMonAmalgamationZapper
  , (,) monAmalgamationSoldier       statusMonAmalgamationSoldier
  , (,) monShoggoth                  statusMonShoggoth
  ]

getStatus :: Id -> Maybe Status
getStatus monId = M.lookup monId monsterStatus

statusWithLevel :: Int -> StatusGrowth -> Status -> Status
statusWithLevel l@_level sg s =
  let gal lens1 lens2 = growthApplyLevelL lens1 lens2 l sg
   in s
    & gal hp hp
    & hp %~ min 1000000000
    & gal speed speed
    & speed %~ min 150
    & gal dodge dodge
    & gal (armor.bash) (armor.bash)
    & gal (armor.bullet) (armor.bullet)
    & gal (armor.cut) (armor.cut)
    & gal (armor.stab) (armor.stab)
    & gal (armor.acid) (armor.acid)
    & gal (armor.heat) (armor.heat)
    & gal (armor.elec) (armor.elec)
    & gal (armor.cold) (armor.cold)
    & gal (armor.pure) (armor.pure)
    & gal (melee.skill) (melee.skill)
    & (melee.skill) %~ min 10
    & gal (melee.dice) (melee.dice)
    & gal (melee.diceSides) (melee.diceSides)
    & growthDamage l sg
    & gal regenerates regenerates

growthApplyLevel :: Int -> Growth -> Int -> Int
growthApplyLevel l@_level (Growth x y) s@_status =
  let s' = fromIntegral s
      l' = fromIntegral $ l - 1
   in floor $ s' * x^(l-1) + y*l'

growthApplyLevelL :: ASetter a b Int Int -> Getting Growth s Growth -> Int -> s -> a -> b
growthApplyLevelL lens1 lens2 l sg s = s
  & lens1 %~ growthApplyLevel l (sg^.lens2)

growthDamage :: Int -> StatusGrowth -> Status -> Status
growthDamage l@_level sg s = s
  & (melee.damage) %~ (\ds ->
    let dgs = sg ^. (melee.damage)
        f :: Damage -> DamageGrowth -> Maybe Damage
        f d dg = if (d^.damageType) /= (dg^.damageType)
                    then Nothing
                    else Just $ d
                      & amount %~ growthApplyLevel l (dg^.amount)
                      & armorPenetration %~ growthApplyLevel l (dg^.armorPenetration)
     in catMaybes $ zipWith f ds dgs
                      )
