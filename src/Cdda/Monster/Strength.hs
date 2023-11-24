{-# LANGUAGE TupleSections #-}
module Cdda.Monster.Strength
  ( allZombieStrength
  , allZombieMap
  , allSkeletonStrength
  , allSkeletonMap
  , getStrength
  ) where

import Define.Core
import Define.Monster

import Data.Bifunctor
import qualified Data.Containers.ListUtils as L
import Cdda.Id.Monster
import Cdda.Id.Harvest

import qualified Data.Map as M
import qualified Data.Maybe as Maybe

strengthMonster :: M.Map Id Strength
strengthMonster = M.fromList $ map (second Strength)
  [ (,) monZombie 1
  , (,) monZombiePrisoner 1
  , (,) monZombieResortDancer 1
  , (,) monZombieResortBouncer 1
  , (,) monZombieResortStaff 1
  , (,) monZombieScientist 1
  , (,) monZombieReenactor 1
  , (,) monZombieCrawler 1
  , (,) monZombieCop 1
  , (,) monZombieFat 1
  , (,) monZombiePrisonerFat 1
  , (,) monZombieRot 1
  , (,) monZombieMedical 1
  , (,) monZombieWretched 1
  , (,) monBoomer 1
  , (,) monZombieRust 1
  , (,) monZombieLabsecurity 1
  , (,) monZombieSwimmerBase 1
  , (,) monZombieSwimmer 3
  , (,) monZombieTechnician 3
  , (,) monZombieMiner 1
  , (,) monZombieThorny 3
  , (,) monZombieStatic 2
  , (,) monZombieAcidic 3
  , (,) monZombieSpitter 5
  , (,) monZombieDogAcidic 3
  , (,) monZombieDogBruteAcidic 3
  , (,) monMeatCocoonTiny 1
  , (,) monMeatCocoonSmall 1
  , (,) monMeatCocoonMed 1
  , (,) monMeatCocoonLarge 1
  , (,) monAmalgamationSwarmer 1
  , (,) monAmalgamationSpotter 1
  , (,) monAmalgamationCorroder 1
  , (,) monAmalgamationJumper 1
  , (,) monAmalgamationZapper 1
  , (,) monAmalgamationSoldier 1
  , (,) monZombieChildScorched 1
  , (,) monZombieFiend 1
  , (,) monZombieScorched 1
  , (,) monZombieAnklebiter 1
  , (,) monZombieChild 1
  , (,) monZombieCreepy 1
  , (,) monZombieShriekling 1
  , (,) monZombieSnotgobbler 1
  , (,) monZombieSproglodyte 1
  , (,) monZombieWaif 1
  , (,) monZombieWretch 1
  , (,) monZombieBruteShocker 8
  , (,) monZombieElectric 5
  , (,) monZombieNullfield 10
  , (,) monBoomerHuge 5
  , (,) monGasZombie 10
  , (,) monZombieGasbag 10
  , (,) monZombieRelaxGasbag 1
  , (,) monZombieTearGasbag 1
  , (,) monBoomerGlutton 10
  , (,) monBoomerClaymore 1
  , (,) monZombieShell 4
  , (,) monZombiePlated 4
  , (,) monZombieUrchin 4
  , (,) monZombieHammerHands 5
  , (,) monCrawler 1
  , (,) monDevourerLabSec 10
  , (,) monZombieCrushedGiant 1
  , (,) monZombieGasbagImmobile 1
  , (,) monZombieGasbagCrawler 1
  , (,) monZombieGasbagImpaler 1
  , (,) monZombieScissorlimbs 1
  , (,) monZombieHangingInnards 1
  , (,) monZombieGiantHeart 1
  , (,) monZombieLivingWall 1
  , (,) monZombiePhaseSkulker 1
  , (,) monZombiePhaseShrike 1
  , (,) monFrogMother 1
  , (,) monTadpoleGrabber 1
  , (,) monZombullfrog 1
  , (,) monZombieBiter 1
  , (,) monZombieBrainless 1
  , (,) monAfsHeadlessHorror 1
  , (,) monZombieBrute 5
  , (,) monZombieBruteGrappler 8
  , (,) monZombieBruteNinja 7
  , (,) monZombieEars 2
  , (,) monZombieNemesis 20
  , (,) monZombieGrabber 5
  , (,) monZombieGrappler 8
  , (,) monZombieHollow 10
  , (,) monZombieHunter 8
  , (,) monZombieJackson 1
  , (,) monZombieMancroc 8
  , (,) monZombieMaster 10
  , (,) monZombieNecro 10
  , (,) monZombieNecroBoomer 5
  , (,) monZombieRunner 3
  , (,) monZombieRegenerating 5
  , (,) monZombieScreecher 3
  , (,) monZombieShady 2
  , (,) monShia 1
  , (,) monZombieShrieker 2
  , (,) monZombieSkull 2
  , (,) monZombieSmoker 3
  , (,) monSmokerBrute 7
  , (,) monZeriatric 1
  , (,) monZombiePrisonerBrute 5
  , (,) monZombiePrisonerTough 2
  , (,) monCharredNightmare 3
  , (,) monIrradiatedWanderer1 1
  , (,) monIrradiatedWanderer2 1
  , (,) monIrradiatedWanderer3 1
  , (,) monIrradiatedWanderer4 1
  , (,) monZombieSoldier 2
  , (,) monZombieSoldierBlackops1 2
  , (,) monZombieSoldierBlackops2 5
  , (,) monZombieSoldierAcid1 3
  , (,) monZombieSoldierAcid2 5
  , (,) monZombieKevlar1 5
  , (,) monZombieKevlar2 10
  , (,) monZombieMilitaryPilot 2
  , (,) monZombieMilbasePersonnel 2
  , (,) monZombieSailor 2
  , (,) monZombieOfficer 2
  , (,) monZombieMarine 2
  , (,) monZombieMarineUpgrade 5
  , (,) monZombieFlamer 2
  , (,) monZombieArmored 1
  , (,) monZombieBioOp 2
  , (,) monZombieBioOp2 3
  , (,) monZombieSurvivor 8
  , (,) monZombieSurvivorElite 10
  , (,) monZombieMedicalBrute 5
  , (,) monZombieMedicalRegenerating 5
  , (,) monZombieMedicalAcidic 3
  , (,) monZombieMedicalPupa 3
  , (,) monZombiePupaMedicalDecoy 3
  , (,) monZombieCrawlerPupaDecoy 3
  , (,) monZombieCrawlerPupa 3
  , (,) monZombiePupaDecoy 3
  , (,) monZombiePupa 3
  , (,) monBrutePupaDecoy 3
  , (,) monBrutePupa 3
  , (,) monHulkPupaDecoy 5
  , (,) monHulkPupa 5
  , (,) monZombiePupaDecoyShady 5
  , (,) monZombiePupaShady 5
  , (,) monBeekeeper 1
  , (,) monZombieDancer 1
  , (,) monZombieFireman 3
  , (,) monZombieHazmat 5
  , (,) monZombieSwat 3
  , (,) monZombieTough 2
  , (,) monZombieWinged 3
  , (,) monZombieBruteWinged 6
  , (,) monSpawnRaptor 2
  , (,) monSpawnRaptorShady 3
  , (,) monSpawnRaptorUnstable 4
  , (,) monSpawnRaptorElectric 4
  , (,) monSkeleton 3
  , (,) monSkeletonMedical 3
  , (,) monSkeletonBrute 8
  , (,) monSkeletonElectric 9
  , (,) monZombieCorrosive 9
  , (,) monZombiePredator 9
  , (,) monSkeletonMaster 15
  , (,) monSkeletonNecro 15
  , (,) monSkeletonHulk 15
  , (,) monDevourer 10
  , (,) monZombieHulk 10
  , (,) monShoggoth 30
  ]

allZombieList :: [Id]
allZombieList =
  [ monZombie
  , monZombiePrisoner
  , monZombieResortDancer
  , monZombieResortBouncer
  , monZombieResortStaff
  , monZombieScientist
  , monZombieReenactor
  , monZombieCrawler
  , monZombieCop
  , monZombieFat
  , monZombiePrisonerFat
  , monZombieRot
  , monZombieMedical
  , monZombieWretched
  , monZombieRust
  , monZombieLabsecurity
  , monZombieSwimmerBase
  , monZombieSwimmer
  , monZombieTechnician
  , monZombieMiner
  , monZombieThorny
  , monZombieStatic
  , monZombieAcidic
  , monZombieSpitter
  , monZombieDogAcidic
  , monZombieDogBruteAcidic
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
  , monZombieBruteShocker
  , monZombieElectric
  , monZombieNullfield
  , monGasZombie
  , monZombieGasbag
  , monZombieRelaxGasbag
  , monZombieTearGasbag
  , monZombieShell
  , monZombiePlated
  , monZombieUrchin
  , monZombieHammerHands
  , monDevourerLabSec
  , monZombieCrushedGiant
  , monZombieGasbagImmobile
  , monZombieGasbagCrawler
  , monZombieGasbagImpaler
  , monZombieScissorlimbs
  , monZombieHangingInnards
  , monZombieGiantHeart
  , monZombieLivingWall
  , monZombiePhaseSkulker
  , monZombiePhaseShrike
  , monZombullfrog
  , monZombieBiter
  , monZombieBrainless
  , monZombieBrute
  , monZombieBruteGrappler
  , monZombieBruteNinja
  , monZombieEars
  , monZombieNemesis
  , monZombieGrabber
  , monZombieGrappler
  , monZombieHollow
  , monZombieHunter
  , monZombieJackson
  , monZombieMancroc
  , monZombieMaster
  , monZombieNecro
  , monZombieNecroBoomer
  , monZombieRunner
  , monZombieRegenerating
  , monZombieScreecher
  , monZombieShady
  , monZombieShrieker
  , monZombieSkull
  , monZombieSmoker
  , monSmokerBrute
  , monZombiePrisonerBrute
  , monZombiePrisonerTough
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
  , monZombieMedicalRegenerating
  , monZombieMedicalAcidic
  , monZombieMedicalPupa
  , monZombiePupaMedicalDecoy
  , monZombieCrawlerPupaDecoy
  , monZombieCrawlerPupa
  , monZombiePupaDecoy
  , monZombiePupa
  , monBrutePupaDecoy
  , monBrutePupa
  , monHulkPupaDecoy
  , monHulkPupa
  , monZombiePupaDecoyShady
  , monZombiePupaShady
  , monBeekeeper
  , monZombieFireman
  , monZombieHazmat
  , monZombieSwat
  , monZombieTough
  , monZombieWinged
  , monZombieBruteWinged
  , monZombieCorrosive
  , monZombiePredator
  , monDevourer
  , monZombieHulk
  , monShoggoth
  ]

allZombieStrength :: [Strength]
allZombieStrength = L.nubOrdOn runStrength $ Maybe.mapMaybe getStrength allZombieList
  where
    runStrength (Strength s) = s

allZombieMap :: M.Map Id Strength
allZombieMap = M.fromList $ Maybe.mapMaybe f allZombieList
  where
    f i = (i,) <$> M.lookup i strengthMonster

allSkeltonList :: [Id]
allSkeltonList =
  [ monSkeleton
  , monSkeletonMedical
  , monSkeletonBrute
  , monSkeletonElectric
  , monSkeletonMaster
  , monSkeletonNecro
  , monSkeletonHulk
  ]

allSkeletonStrength :: [Strength]
allSkeletonStrength = L.nubOrdOn runStrength $ Maybe.mapMaybe getStrength allSkeltonList
  where
    runStrength (Strength s) = s

allSkeletonMap :: M.Map Id Strength
allSkeletonMap = M.fromList $ Maybe.mapMaybe f allSkeltonList
  where
    f i = (i,) <$> M.lookup i strengthMonster

allStrength :: [Strength]
allStrength = undefined

getStrength :: Id -> Maybe Strength
getStrength modId = M.lookup modId strengthMonster
