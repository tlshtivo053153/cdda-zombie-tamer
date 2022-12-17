{-# LANGUAGE MultiWayIf #-}
module Cdda.Monster.Exp
  ( calcExp
  , itemExp ) where

import Prelude hiding (exp)
import Define.Core
import Define.Monster
import Define.MakeFields

import Cdda.Id.Item
import Cdda.Id.Monster

import Control.Lens

--monsterNormalExp =
--  [ (,) monZombieAcidic
--  , (,) monZombieCorrosive
--  , (,) monZombieSpitter
--  , (,) monZombieWretched
--  , (,) monZombieDogAcidic
--  , (,) monZombieDogBruteAcidic
--  , (,) monMeatCocoonTiny
--  , (,) monMeatCocoonSmall
--  , (,) monMeatCocoonMed
--  , (,) monMeatCocoonLarge
--  , (,) monAmalgamationSwarmer
--  , (,) monAmalgamationSpotter
--  , (,) monAmalgamationCorroder
--  , (,) monAmalgamationJumper
--  , (,) monAmalgamationZapper
--  , (,) monAmalgamationSoldier
--  , (,) monZombieChildScorched
--  , (,) monZombieFiend
--  , (,) monZombieScorched
--  , (,) monZombieAnklebiter
--  , (,) monZombieChild
--  , (,) monZombieCreepy
--  , (,) monZombieShriekling
--  , (,) monZombieSnotgobbler
--  , (,) monZombieSproglodyte
--  , (,) monZombieWaif
--  , (,) monZombieWretch
--  , (,) monSkeletonMaster
--  , (,) monSkeletonNecro
--  , (,) monZombieBruteShocker
--  , (,) monZombieElectric
--  , (,) monZombieNullfield
--  , (,) monZombieStatic
--  , (,) monBoomer
--  , (,) monBoomerHuge
--  , (,) monGasZombie
--  , (,) monZombieGasbag
--  , (,) monZombieRelaxGasbag
--  , (,) monZombieTearGasbag
--  , (,) monBoomerGlutton
--  , (,) monBoomerClaymore
--  , (,) monZombieRust
--  , (,) monZombieShell
--  , (,) monZombiePlated
--  , (,) monZombieUrchin
--  , (,) monZombieHammerHands
--  , (,) monCrawler
--  , (,) monDevourer
--  , (,) monDevourerLabSec
--  , (,) monZombieCrushedGiant
--  , (,) monZombieGasbagImmobile
--  , (,) monZombieGasbagCrawler
--  , (,) monZombieGasbagImpaler
--  , (,) monZombieScissorlimbs
--  , (,) monZombieHangingInnards
--  , (,) monZombieGiantHeart
--  , (,) monZombieLivingWall
--  , (,) monZombieScientist
--  , (,) monZombieLabsecurity
--  , (,) monZombiePhaseSkulker
--  , (,) monZombiePhaseShrike
--  , (,) monFrogMother
--  , (,) monTadpoleGrabber
--  , (,) monZombullfrog
--  , (,) monZombieBiter
--  , (,) monZombieBrainless
--  , (,) monAfsHeadlessHorror
--  , (,) monZombieBrute
--  , (,) monZombieBruteGrappler
--  , (,) monZombieBruteNinja
--  , (,) monZombieEars
--  , (,) monZombieNemesis
--  , (,) monZombieGrabber
--  , (,) monZombieGrappler
--  , (,) monZombieHollow
--  , (,) monZombieHulk
--  , (,) monZombieHunter
--  , (,) monZombieJackson
--  , (,) monZombieMancroc
--  , (,) monZombieMaster
--  , (,) monZombieNecro
--  , (,) monZombieNecroBoomer
--  , (,) monZombieRunner
--  , (,) monZombieRegenerating
--  , (,) monZombiePredator
--  , (,) monZombieScreecher
--  , (,) monZombieShady
--  , (,) monShia
--  , (,) monZombieShrieker
--  , (,) monZombieSkull
--  , (,) monZombieSmoker
--  , (,) monSmokerBrute
--  , (,) monZombieSwimmerBase
--  , (,) monZombieSwimmer
--  , (,) monZombieTechnician
--  , (,) monZombieMiner
--  , (,) monZombieThorny
--  , (,) monZombieReenactor
--  , (,) monZeriatric
--  , (,) monZombiePrisoner
--  , (,) monZombiePrisonerBrute
--  , (,) monZombiePrisonerFat
--  , (,) monZombiePrisonerTough
--  , (,) monCharredNightmare
--  , (,) monIrradiatedWanderer1
--  , (,) monIrradiatedWanderer2
--  , (,) monIrradiatedWanderer3
--  , (,) monIrradiatedWanderer4
--  , (,) monSkeleton
--  , (,) monSkeletonBrute
--  , (,) monSkeletonElectric
--  , (,) monSkeletonHulk
--  , (,) monZombieSoldier
--  , (,) monZombieSoldierBlackops1
--  , (,) monZombieSoldierBlackops2
--  , (,) monZombieSoldierAcid1
--  , (,) monZombieSoldierAcid2
--  , (,) monZombieKevlar1
--  , (,) monZombieKevlar2
--  , (,) monZombieMilitaryPilot
--  , (,) monZombieMilbasePersonnel
--  , (,) monZombieSailor
--  , (,) monZombieOfficer
--  , (,) monZombieMarine
--  , (,) monZombieMarineUpgrade
--  , (,) monZombieFlamer
--  , (,) monZombieArmored
--  , (,) monZombieBioOp
--  , (,) monZombieBioOp2
--  , (,) monZombieSurvivor
--  , (,) monZombieSurvivorElite
--  , (,) monZombieMedical
--  , (,) monZombieMedicalBrute
--  , (,) monZombieMedicalRegenerating
--  , (,) monSkeletonMedical
--  , (,) monZombieMedicalAcidic
--  , (,) monZombieMedicalPupa
--  , (,) monZombiePupaMedicalDecoy
--  , (,) monZombieCrawlerPupaDecoy
--  , (,) monZombieCrawlerPupa
--  , (,) monZombiePupaDecoy
--  , (,) monZombiePupa
--  , (,) monBrutePupaDecoy
--  , (,) monBrutePupa
--  , (,) monHulkPupaDecoy
--  , (,) monHulkPupa
--  , (,) monZombiePupaDecoyShady
--  , (,) monZombiePupaShady
--  , (,) monBeekeeper
--  , (,) monZombie
--  , (,) monZombieCop
--  , (,) monZombieCrawler
--  , (,) monZombieDancer
--  , (,) monZombieFat
--  , (,) monZombieFireman
--  , (,) monZombieHazmat
--  , (,) monZombieRot
--  , (,) monZombieSwat
--  , (,) monZombieTough
--  , (,) monZombieResortDancer
--  , (,) monZombieResortBouncer
--  , (,) monZombieResortStaff
--  , (,) monZombieWinged
--  , (,) monZombieBruteWinged
--  , (,) monSpawnRaptor
--  , (,) monSpawnRaptorShady
--  , (,) monSpawnRaptorUnstable
--  , (,) monSpawnRaptorElectric
--  , (,) monShoggoth
--  ]

initialExp :: Monster -> Int
initialExp mon =
  let (Strength str) = mon^.strength
   in (floor :: Rational -> Int) $ 10 * 1.12^(str-1)

--calcExp :: Growth -> Rational -> Int -> Int
--calcExp (Growth x y) initExp l = floor $ initExp*x^(l-1) + y*fromIntegral (l-1)

calcExp :: Monster -> Int -> Int
calcExp mon l =
  let (Growth x y) = mon^.growth.exp
   in floor $ fromIntegral (initialExp mon) *x^(l-1) + y*fromIntegral (l-1)

itemExp :: Id -> Int
itemExp idItem =
  if
     | idItem == idTaintedMeatPremium -> 15
     | idItem == idTaintedMeatHighPremium -> 3000
     | idItem == idTaintedMarrowPremium -> 120
     | idItem == idTaintedMarrowHighPremium -> 24000
     | otherwise -> 0
