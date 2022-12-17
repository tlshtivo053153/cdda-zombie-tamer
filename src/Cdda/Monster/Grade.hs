module Cdda.Monster.Grade
  ( getGrade
  ) where

import Define.Core

import Cdda.Id.Monster

import Data.Bifunctor
import qualified Data.Map as M

gradeMeat1Monster :: [(Id, Grade)]
gradeMeat1Monster = map (second $ Grade GradeMeat1)
  [ (,) monZombie 0
  , (,) monZombiePrisoner 0
  , (,) monZombieResortDancer 0
  , (,) monZombieResortBouncer 0
  , (,) monZombieResortStaff 0
  , (,) monZombieScientist 0
  , (,) monZombieReenactor 0
  , (,) monZombieCrawler 0
  , (,) monZombieCop 0
  , (,) monZombieFat 0
  , (,) monZombiePrisonerFat 0
  , (,) monZombieRot 0
  , (,) monZombieMedical 0
  , (,) monZombieWretched 5
  , (,) monBoomer 0
  , (,) monZombieRust 0
  , (,) monZombieLabsecurity 0
  , (,) monZombieSwimmerBase 0
  , (,) monZombieSwimmer 1
  , (,) monZombieTechnician 10
  , (,) monZombieMiner 1
  , (,) monZombieThorny 10
  , (,) monZombieStatic 5
  ]

gradeMeat2Monster :: [(Id, Grade)]
gradeMeat2Monster = map (second $ Grade GradeMeat2)
  [ (,) monZombieAcidic 1
  , (,) monZombieSpitter 1
  , (,) monZombieDogAcidic 1
  , (,) monZombieDogBruteAcidic 1
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
  , (,) monZombieBruteShocker 1
  , (,) monZombieElectric 1
  , (,) monZombieNullfield 1
  , (,) monBoomerHuge 1
  , (,) monGasZombie 1
  , (,) monZombieGasbag 1
  , (,) monZombieRelaxGasbag 1
  , (,) monZombieTearGasbag 1
  , (,) monBoomerGlutton 1
  , (,) monBoomerClaymore 1
  , (,) monZombieShell 1
  , (,) monZombiePlated 1
  , (,) monZombieUrchin 1
  , (,) monZombieHammerHands 1
  , (,) monCrawler 1
  , (,) monDevourerLabSec 1
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
  , (,) monZombieBrute 1
  , (,) monZombieBruteGrappler 1
  , (,) monZombieBruteNinja 1
  , (,) monZombieEars 1
  , (,) monZombieNemesis 1
  , (,) monZombieGrabber 1
  , (,) monZombieGrappler 1
  , (,) monZombieHollow 2
  , (,) monZombieHunter 1
  , (,) monZombieJackson 1
  , (,) monZombieMancroc 1
  , (,) monZombieMaster 1
  , (,) monZombieNecro 1
  , (,) monZombieNecroBoomer 1
  , (,) monZombieRunner 1
  , (,) monZombieRegenerating 1
  , (,) monZombieScreecher 1
  , (,) monZombieShady 1
  , (,) monShia 1
  , (,) monZombieShrieker 1
  , (,) monZombieSkull 1
  , (,) monZombieSmoker 1
  , (,) monSmokerBrute 1
  , (,) monZeriatric 1
  , (,) monZombiePrisonerBrute 1
  , (,) monZombiePrisonerTough 1
  , (,) monCharredNightmare 1
  , (,) monIrradiatedWanderer1 1
  , (,) monIrradiatedWanderer2 1
  , (,) monIrradiatedWanderer3 1
  , (,) monIrradiatedWanderer4 1
  , (,) monZombieSoldier 1
  , (,) monZombieSoldierBlackops1 1
  , (,) monZombieSoldierBlackops2 1
  , (,) monZombieSoldierAcid1 1
  , (,) monZombieSoldierAcid2 1
  , (,) monZombieKevlar1 1
  , (,) monZombieKevlar2 1
  , (,) monZombieMilitaryPilot 1
  , (,) monZombieMilbasePersonnel 1
  , (,) monZombieSailor 1
  , (,) monZombieOfficer 1
  , (,) monZombieMarine 1
  , (,) monZombieMarineUpgrade 1
  , (,) monZombieFlamer 1
  , (,) monZombieArmored 1
  , (,) monZombieBioOp 1
  , (,) monZombieBioOp2 1
  , (,) monZombieSurvivor 1
  , (,) monZombieSurvivorElite 1
  , (,) monZombieMedicalBrute 1
  , (,) monZombieMedicalRegenerating 1
  , (,) monZombieMedicalAcidic 1
  , (,) monZombieMedicalPupa 1
  , (,) monZombiePupaMedicalDecoy 1
  , (,) monZombieCrawlerPupaDecoy 1
  , (,) monZombieCrawlerPupa 1
  , (,) monZombiePupaDecoy 1
  , (,) monZombiePupa 1
  , (,) monBrutePupaDecoy 1
  , (,) monBrutePupa 1
  , (,) monHulkPupaDecoy 1
  , (,) monHulkPupa 1
  , (,) monZombiePupaDecoyShady 1
  , (,) monZombiePupaShady 1
  , (,) monBeekeeper 1
  , (,) monZombieDancer 1
  , (,) monZombieFireman 1
  , (,) monZombieHazmat 1
  , (,) monZombieSwat 1
  , (,) monZombieTough 1
  , (,) monZombieWinged 1
  , (,) monZombieBruteWinged 1
  , (,) monSpawnRaptor 1
  , (,) monSpawnRaptorShady 1
  , (,) monSpawnRaptorUnstable 1
  , (,) monSpawnRaptorElectric 1
  ]

gradeMarrow1Monster :: [(Id, Grade)]
gradeMarrow1Monster = map (second $ Grade GradeMarrow1)
  [ (,) monSkeleton 1
  , (,) monSkeletonMedical 1
  , (,) monSkeletonBrute 2
  , (,) monSkeletonElectric 2
  , (,) monZombieCorrosive 1
  , (,) monZombiePredator 2
  ]

gradeMarrow2Monster :: [(Id, Grade)]
gradeMarrow2Monster = map (second $ Grade GradeMarrow2)
  [ (,) monSkeletonMaster 1
  , (,) monSkeletonNecro 1
  , (,) monSkeletonHulk 1
  , (,) monDevourer 1
  , (,) monZombieHulk 1
  , (,) monShoggoth 10
  ]

gradeAllMonster :: M.Map Id Grade
gradeAllMonster = M.fromList $
  gradeMeat1Monster
  ++ gradeMeat2Monster
  ++ gradeMarrow1Monster
  ++ gradeMarrow2Monster

getGrade :: Id -> Maybe Grade
getGrade monId = M.lookup monId gradeAllMonster
