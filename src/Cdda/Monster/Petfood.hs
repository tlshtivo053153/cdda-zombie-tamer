module Cdda.Monster.Petfood
  ( getPetFood
  , getFriendCost
  , costToPetfood
  ) where

import Prelude hiding ( exp )
import Define.Core
import Define.Monster
import Define.MakeFields

import Cdda.Id.Monster

import Cdda.Item

import Cdda.Monster.Strength
import Cdda.Monster.Growth

import Data.Maybe
import qualified Data.Map as M

import Control.Lens

--foodMeatMonster :: [Id]
--foodMeatMonster =
--  [ monZombie
--  , monZombiePrisoner
--  , monZombieResortDancer
--  , monZombieResortBouncer
--  , monZombieResortStaff
--  , monZombieScientist
--  , monZombieReenactor
--  , monZombieCrawler
--  , monZombieCop
--  , monZombieFat
--  , monZombiePrisonerFat
--  , monZombieRot
--  , monZombieMedical
--  , monZombieWretched
--  , monBoomer
--  , monZombieRust
--  , monZombieLabsecurity
--  , monZombieSwimmerBase
--  , monZombieSwimmer
--  , monZombieTechnician
--  , monZombieMiner
--  , monZombieThorny
--  , monZombieStatic
--  ]
--
--foodMeatHighMonster :: [Id]
--foodMeatHighMonster =
--  [ monZombieAcidic
--  , monZombieSpitter
--  , monZombieDogAcidic
--  , monZombieDogBruteAcidic
--  , monMeatCocoonTiny
--  , monMeatCocoonSmall
--  , monMeatCocoonMed
--  , monMeatCocoonLarge
--  , monAmalgamationSwarmer
--  , monAmalgamationSpotter
--  , monAmalgamationCorroder
--  , monAmalgamationJumper
--  , monAmalgamationZapper
--  , monAmalgamationSoldier
--  , monZombieChildScorched
--  , monZombieFiend
--  , monZombieScorched
--  , monZombieAnklebiter
--  , monZombieChild
--  , monZombieCreepy
--  , monZombieShriekling
--  , monZombieSnotgobbler
--  , monZombieSproglodyte
--  , monZombieWaif
--  , monZombieWretch
--  , monZombieBruteShocker
--  , monZombieElectric
--  , monZombieNullfield
--  , monBoomerHuge
--  , monGasZombie
--  , monZombieGasbag
--  , monZombieRelaxGasbag
--  , monZombieTearGasbag
--  , monBoomerGlutton
--  , monBoomerClaymore
--  , monZombieShell
--  , monZombiePlated
--  , monZombieUrchin
--  , monZombieHammerHands
--  , monCrawler
--  , monDevourerLabSec
--  , monZombieCrushedGiant
--  , monZombieGasbagImmobile
--  , monZombieGasbagCrawler
--  , monZombieGasbagImpaler
--  , monZombieScissorlimbs
--  , monZombieHangingInnards
--  , monZombieGiantHeart
--  , monZombieLivingWall
--  , monZombiePhaseSkulker
--  , monZombiePhaseShrike
--  , monFrogMother
--  , monTadpoleGrabber
--  , monZombullfrog
--  , monZombieBiter
--  , monZombieBrainless
--  , monAfsHeadlessHorror
--  , monZombieBrute
--  , monZombieBruteGrappler
--  , monZombieBruteNinja
--  , monZombieEars
--  , monZombieNemesis
--  , monZombieGrabber
--  , monZombieGrappler
--  , monZombieHollow
--  , monZombieHunter
--  , monZombieJackson
--  , monZombieMancroc
--  , monZombieMaster
--  , monZombieNecro
--  , monZombieNecroBoomer
--  , monZombieRunner
--  , monZombieRegenerating
--  , monZombieScreecher
--  , monZombieShady
--  , monShia
--  , monZombieShrieker
--  , monZombieSkull
--  , monZombieSmoker
--  , monSmokerBrute
--  , monZeriatric
--  , monZombiePrisonerBrute
--  , monZombiePrisonerTough
--  , monCharredNightmare
--  , monIrradiatedWanderer1
--  , monIrradiatedWanderer2
--  , monIrradiatedWanderer3
--  , monIrradiatedWanderer4
--  , monZombieSoldier
--  , monZombieSoldierBlackops1
--  , monZombieSoldierBlackops2
--  , monZombieSoldierAcid1
--  , monZombieSoldierAcid2
--  , monZombieKevlar1
--  , monZombieKevlar2
--  , monZombieMilitaryPilot
--  , monZombieMilbasePersonnel
--  , monZombieSailor
--  , monZombieOfficer
--  , monZombieMarine
--  , monZombieMarineUpgrade
--  , monZombieFlamer
--  , monZombieArmored
--  , monZombieBioOp
--  , monZombieBioOp2
--  , monZombieSurvivor
--  , monZombieSurvivorElite
--  , monZombieMedicalBrute
--  , monZombieMedicalRegenerating
--  , monZombieMedicalAcidic
--  , monZombieMedicalPupa
--  , monZombiePupaMedicalDecoy
--  , monZombieCrawlerPupaDecoy
--  , monZombieCrawlerPupa
--  , monZombiePupaDecoy
--  , monZombiePupa
--  , monBrutePupaDecoy
--  , monBrutePupa
--  , monHulkPupaDecoy
--  , monHulkPupa
--  , monZombiePupaDecoyShady
--  , monZombiePupaShady
--  , monBeekeeper
--  , monZombieDancer
--  , monZombieFireman
--  , monZombieHazmat
--  , monZombieSwat
--  , monZombieTough
--  , monZombieWinged
--  , monZombieBruteWinged
--  , monSpawnRaptor
--  , monSpawnRaptorShady
--  , monSpawnRaptorUnstable
--  , monSpawnRaptorElectric
--  ]
--
--foodMarrowMonster :: [Id]
--foodMarrowMonster =
--  [ monSkeleton
--  , monSkeletonMedical
--  , monSkeletonBrute
--  ]
--
--foodMarrowHighMonster :: [Id]
--foodMarrowHighMonster =
--  [ monSkeletonMaster
--  , monSkeletonNecro
--  , monSkeletonElectric
--  , monSkeletonHulk
--  , monZombiePredator
--  , monDevourer
--  , monZombieCorrosive
--  , monZombieHulk
--  ]
--
--foodNoneMonster :: [Id]
--foodNoneMonster =
--  [ monShoggoth ]
--
--foodMeat :: PetFood
--foodMeat = PetFood
--  [ foodCategoryTMeat1
--  , foodCategoryTMeat2
--  ]
--
--foodMarrow :: PetFood
--foodMarrow = PetFood
--  [ foodCategoryTMarrow1
--  , foodCategoryTMarrow2
--  ]
--
--foodMeatHigh :: PetFood
--foodMeatHigh = PetFood [ foodCategoryTMeat2 ]
--
--foodMarrowHigh :: PetFood
--foodMarrowHigh = PetFood [ foodCategoryTMarrow2 ]
--
--foodMap :: M.Map Id PetFood
--foodMap = M.fromList $ concatMap (\(mons, food) -> zip mons $ repeat food)
--  [ (,) foodMeatMonster foodMeat
--  , (,) foodMeatHighMonster foodMeatHigh
--  , (,) foodMarrowMonster foodMarrow
--  , (,) foodMarrowHighMonster foodMarrowHigh
--  , (,) foodNoneMonster $ PetFood []
--  ]

foodAll :: [FoodCategory]
foodAll =
  [ foodCategoryTMeat1
  , foodCategoryTMarrow1
  , foodCategoryTMeat2
  , foodCategoryTMarrow2
  ]

--getPetFood :: Id -> PetFood
--getPetFood monId = fromMaybe (PetFood []) $ M.lookup monId foodMap

getPetFood :: Id -> PetFood
getPetFood monId = case getFriendCost monId of
                     Nothing -> PetFood []
                     Just x -> costToPetfood x

getFriendCost :: Id -> Maybe Int
getFriendCost monId = do
  s <- do
    (Strength _s) <- getStrength monId
    return $ _s - 1
  (Growth x y) <- view exp <$> getGrowth monId
  return $ floor $ 10 * x^s + y*fromIntegral s

costToPetfood :: Int -> PetFood
costToPetfood cost | cost <= 15    = PetFood foodAll
                   | cost <= 120   = PetFood $ take 1 foodAll
                   | cost <= 3000  = PetFood $ take 2 foodAll
                   | cost <= 24000 = PetFood $ take 3 foodAll
                   | otherwise     = PetFood []

