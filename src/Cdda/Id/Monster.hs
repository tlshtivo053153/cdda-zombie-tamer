{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.Monster where

import Data.Text (Text)

import Define.Core

allMonster :: [Id]
allMonster =
  [ monZombieAcidic
  , monZombieCorrosive
  , monZombieSpitter
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
  , monSkeletonMaster
  , monSkeletonNecro
  , monZombieBruteShocker
  , monZombieElectric
  , monZombieNullfield
  , monZombieStatic
  , monBoomer
  , monBoomerHuge
  , monGasZombie
  , monZombieGasbag
  , monZombieRelaxGasbag
  , monZombieTearGasbag
  , monBoomerGlutton
  , monBoomerClaymore
  , monZombieRust
  , monZombieShell
  , monZombiePlated
  , monZombieUrchin
  , monZombieHammerHands
  , monCrawler
  , monDevourer
  , monDevourerLabSec
  , monZombieCrushedGiant
  , monZombieGasbagImmobile
  , monZombieGasbagCrawler
  , monZombieGasbagImpaler
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
  , monZombieBrute
  , monZombieBruteGrappler
  , monZombieBruteNinja
  , monZombieEars
  , monZombieNemesis
  , monZombieGrabber
  , monZombieGrappler
  , monZombieHollow
  , monZombieHulk
  , monZombieHunter
  , monZombieJackson
  , monZombieMancroc
  , monZombieMaster
  , monZombieNecro
  , monZombieNecroBoomer
  , monZombieRunner
  , monZombieRegenerating
  , monZombiePredator
  , monZombieScreecher
  , monZombieShady
  , monShia
  , monZombieShrieker
  , monZombieSkull
  , monZombieSmoker
  , monSmokerBrute
  , monZombieSwimmerBase
  , monZombieSwimmer
  , monZombieTechnician
  , monZombieMiner
  , monZombieThorny
  , monZombieReenactor
  , monZeriatric
  , monZombiePrisoner
  , monZombiePrisonerBrute
  , monZombiePrisonerFat
  , monZombiePrisonerTough
  , monCharredNightmare
  , monIrradiatedWanderer1
  , monIrradiatedWanderer2
  , monIrradiatedWanderer3
  , monIrradiatedWanderer4
  , monSkeleton
  , monSkeletonBrute
  , monSkeletonElectric
  , monSkeletonHulk
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
  , monZombieMedical
  , monZombieMedicalBrute
  , monZombieMedicalRegenerating
  , monSkeletonMedical
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
  , monZombie
  , monZombieCop
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
  , monZombieBruteWinged
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

allFriendMonster :: [Id]
allFriendMonster =
  [ monZombieAcidic
  , monZombieCorrosive
  , monZombieSpitter
  , monZombieWretched
  , monSkeletonMaster
  , monSkeletonNecro
  , monZombieBruteShocker
  , monZombieElectric
  , monZombieNullfield
  , monZombieStatic
  , monBoomer
  , monBoomerHuge
  , monGasZombie
  , monBoomerGlutton
  , monZombieRust
  , monZombieShell
  , monZombiePlated
  , monZombieUrchin
  , monZombieHammerHands
  , monDevourer
  , monZombieScientist
  , monZombieLabsecurity
  , monZombieBiter
  , monZombieBrute
  , monZombieBruteGrappler
  , monZombieBruteNinja
  , monZombieNemesis
  , monZombieGrabber
  , monZombieGrappler
  , monZombieHollow
  , monZombieHulk
  , monZombieHunter
  , monZombieMancroc
  , monZombieMaster
  , monZombieNecro
  , monZombieNecroBoomer
  , monZombieRunner
  , monZombieRegenerating
  , monZombiePredator
  , monZombieScreecher
  , monZombieShady
  , monZombieShrieker
  , monZombieSkull
  , monZombieSmoker
  , monSmokerBrute
  , monZombieSwimmerBase
  , monZombieSwimmer
  , monZombieTechnician
  , monZombieMiner
  , monZombieThorny
  , monZombiePrisoner
  , monZombiePrisonerBrute
  , monZombiePrisonerFat
  , monZombiePrisonerTough
  , monSkeleton
  , monSkeletonBrute
  , monSkeletonElectric
  , monSkeletonHulk
  , monZombieKevlar1
  , monZombieKevlar2
  , monZombieMedical
  , monZombieMedicalBrute
  , monZombieMedicalRegenerating
  , monSkeletonMedical
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
  , monZombie
  , monZombieCop
  , monZombieCrawler
  , monZombieFat
  , monZombieTough
  , monZombieResortDancer
  , monZombieResortBouncer
  , monZombieResortStaff
  , monZombieWinged
  , monZombieBruteWinged
  , monShoggoth
  ]

monZombieAcidic :: Id
monZombieCorrosive :: Id
monZombieSpitter :: Id
monZombieWretched :: Id
monZombieDogAcidic :: Id
monZombieDogBruteAcidic :: Id
monMeatCocoonTiny :: Id
monMeatCocoonSmall :: Id
monMeatCocoonMed :: Id
monMeatCocoonLarge :: Id
monAmalgamationSwarmer :: Id
monAmalgamationSpotter :: Id
monAmalgamationCorroder :: Id
monAmalgamationJumper :: Id
monAmalgamationZapper :: Id
monAmalgamationSoldier :: Id
monZombieChildScorched :: Id
monZombieFiend :: Id
monZombieScorched :: Id
monZombieAnklebiter :: Id
monZombieChild :: Id
monZombieCreepy :: Id
monZombieShriekling :: Id
monZombieSnotgobbler :: Id
monZombieSproglodyte :: Id
monZombieWaif :: Id
monZombieWretch :: Id
monSkeletonMaster :: Id
monSkeletonNecro :: Id
monZombieBruteShocker :: Id
monZombieElectric :: Id
monZombieNullfield :: Id
monZombieStatic :: Id
monBoomer :: Id
monBoomerHuge :: Id
monGasZombie :: Id
monZombieGasbag :: Id
monZombieRelaxGasbag :: Id
monZombieTearGasbag :: Id
monBoomerGlutton :: Id
monBoomerClaymore :: Id
monZombieRust :: Id
monZombieShell :: Id
monZombiePlated :: Id
monZombieUrchin :: Id
monZombieHammerHands :: Id
monCrawler :: Id
monDevourer :: Id
monDevourerLabSec :: Id
monZombieCrushedGiant :: Id
monZombieGasbagImmobile :: Id
monZombieGasbagCrawler :: Id
monZombieGasbagImpaler :: Id
monZombieScissorlimbs :: Id
monZombieHangingInnards :: Id
monZombieGiantHeart :: Id
monZombieLivingWall :: Id
monZombieScientist :: Id
monZombieLabsecurity :: Id
monZombiePhaseSkulker :: Id
monZombiePhaseShrike :: Id
monFrogMother :: Id
monTadpoleGrabber :: Id
monZombullfrog :: Id
monZombieBiter :: Id
monZombieBrainless :: Id
monAfsHeadlessHorror :: Id
monZombieBrute :: Id
monZombieBruteGrappler :: Id
monZombieBruteNinja :: Id
monZombieEars :: Id
monZombieNemesis :: Id
monZombieGrabber :: Id
monZombieGrappler :: Id
monZombieHollow :: Id
monZombieHulk :: Id
monZombieHunter :: Id
monZombieJackson :: Id
monZombieMancroc :: Id
monZombieMaster :: Id
monZombieNecro :: Id
monZombieNecroBoomer :: Id
monZombieRunner :: Id
monZombieRegenerating :: Id
monZombiePredator :: Id
monZombieScreecher :: Id
monZombieShady :: Id
monShia :: Id
monZombieShrieker :: Id
monZombieSkull :: Id
monZombieSmoker :: Id
monSmokerBrute :: Id
monZombieSwimmerBase :: Id
monZombieSwimmer :: Id
monZombieTechnician :: Id
monZombieMiner :: Id
monZombieThorny :: Id
monZombieReenactor :: Id
monZeriatric :: Id
monZombiePrisoner :: Id
monZombiePrisonerBrute :: Id
monZombiePrisonerFat :: Id
monZombiePrisonerTough :: Id
monCharredNightmare :: Id
monIrradiatedWanderer1 :: Id
monIrradiatedWanderer2 :: Id
monIrradiatedWanderer3 :: Id
monIrradiatedWanderer4 :: Id
monSkeleton :: Id
monSkeletonBrute :: Id
monSkeletonElectric :: Id
monSkeletonHulk :: Id
monZombieSoldier :: Id
monZombieSoldierBlackops1 :: Id
monZombieSoldierBlackops2 :: Id
monZombieSoldierAcid1 :: Id
monZombieSoldierAcid2 :: Id
monZombieKevlar1 :: Id
monZombieKevlar2 :: Id
monZombieMilitaryPilot :: Id
monZombieMilbasePersonnel :: Id
monZombieSailor :: Id
monZombieOfficer :: Id
monZombieMarine :: Id
monZombieMarineUpgrade :: Id
monZombieFlamer :: Id
monZombieArmored :: Id
monZombieBioOp :: Id
monZombieBioOp2 :: Id
monZombieSurvivor :: Id
monZombieSurvivorElite :: Id

monZombieAcidic           = Id "mon_zombie_acidic"
monZombieCorrosive        = Id "mon_zombie_corrosive"
monZombieSpitter          = Id "mon_zombie_spitter"
monZombieWretched         = Id "mon_zombie_wretched"
monZombieDogAcidic        = Id "mon_zombie_dog_acidic"
monZombieDogBruteAcidic   = Id "mon_zombie_dog_brute_acidic"
monMeatCocoonTiny         = Id "mon_meat_cocoon_tiny"
monMeatCocoonSmall        = Id "mon_meat_cocoon_small"
monMeatCocoonMed          = Id "mon_meat_cocoon_med"
monMeatCocoonLarge        = Id "mon_meat_cocoon_large"
monAmalgamationSwarmer    = Id "mon_amalgamation_swarmer"
monAmalgamationSpotter    = Id "mon_amalgamation_spotter"
monAmalgamationCorroder   = Id "mon_amalgamation_corroder"
monAmalgamationJumper     = Id "mon_amalgamation_jumper"
monAmalgamationZapper     = Id "mon_amalgamation_zapper"
monAmalgamationSoldier    = Id "mon_amalgamation_soldier"
monZombieChildScorched    = Id "mon_zombie_child_scorched"
monZombieFiend            = Id "mon_zombie_fiend"
monZombieScorched         = Id "mon_zombie_scorched"
monZombieAnklebiter       = Id "mon_zombie_anklebiter"
monZombieChild            = Id "mon_zombie_child"
monZombieCreepy           = Id "mon_zombie_creepy"
monZombieShriekling       = Id "mon_zombie_shriekling"
monZombieSnotgobbler      = Id "mon_zombie_snotgobbler"
monZombieSproglodyte      = Id "mon_zombie_sproglodyte"
monZombieWaif             = Id "mon_zombie_waif"
monZombieWretch           = Id "mon_zombie_wretch"
monSkeletonMaster         = Id "mon_skeleton_master"
monSkeletonNecro          = Id "mon_skeleton_necro"
monZombieBruteShocker     = Id "mon_zombie_brute_shocker"
monZombieElectric         = Id "mon_zombie_electric"
monZombieNullfield        = Id "mon_zombie_nullfield"
monZombieStatic           = Id "mon_zombie_static"
monBoomer                 = Id "mon_boomer"
monBoomerHuge             = Id "mon_boomer_huge"
monGasZombie              = Id "mon_gas_zombie"
monZombieGasbag           = Id "mon_zombie_gasbag"
monZombieRelaxGasbag      = Id "mon_zombie_relax_gasbag"
monZombieTearGasbag       = Id "mon_zombie_tear_gasbag"
monBoomerGlutton          = Id "mon_boomer_glutton"
monBoomerClaymore         = Id "mon_boomer_claymore"
monZombieRust             = Id "mon_zombie_rust"
monZombieShell            = Id "mon_zombie_shell"
monZombiePlated           = Id "mon_zombie_plated"
monZombieUrchin           = Id "mon_zombie_urchin"
monZombieHammerHands      = Id "mon_zombie_hammer_hands"
monCrawler                = Id "mon_crawler"
monDevourer               = Id "mon_devourer"
monDevourerLabSec         = Id "mon_devourer_lab_sec"
monZombieCrushedGiant     = Id "mon_zombie_crushed_giant"
monZombieGasbagImmobile   = Id "mon_zombie_gasbag_immobile"
monZombieGasbagCrawler    = Id "mon_zombie_gasbag_crawler"
monZombieGasbagImpaler    = Id "mon_zombie_gasbag_impaler"
monZombieScissorlimbs     = Id "mon_zombie_scissorlimbs"
monZombieHangingInnards   = Id "mon_zombie_hanging_innards"
monZombieGiantHeart       = Id "mon_zombie_giant_heart"
monZombieLivingWall       = Id "mon_zombie_living_wall"
monZombieScientist        = Id "mon_zombie_scientist"
monZombieLabsecurity      = Id "mon_zombie_labsecurity"
monZombiePhaseSkulker     = Id "mon_zombie_phase_skulker"
monZombiePhaseShrike      = Id "mon_zombie_phase_shrike"
monFrogMother             = Id "mon_frog_mother"
monTadpoleGrabber         = Id "mon_tadpole_grabber"
monZombullfrog            = Id "mon_zombullfrog"
monZombieBiter            = Id "mon_zombie_biter"
monZombieBrainless        = Id "mon_zombie_brainless"
monAfsHeadlessHorror      = Id "mon_afs_headless_horror"
monZombieBrute            = Id "mon_zombie_brute"
monZombieBruteGrappler    = Id "mon_zombie_brute_grappler"
monZombieBruteNinja       = Id "mon_zombie_brute_ninja"
monZombieEars             = Id "mon_zombie_ears"
monZombieNemesis          = Id "mon_zombie_nemesis"
monZombieGrabber          = Id "mon_zombie_grabber"
monZombieGrappler         = Id "mon_zombie_grappler"
monZombieHollow           = Id "mon_zombie_hollow"
monZombieHulk             = Id "mon_zombie_hulk"
monZombieHunter           = Id "mon_zombie_hunter"
monZombieJackson          = Id "mon_zombie_jackson"
monZombieMancroc          = Id "mon_zombie_mancroc"
monZombieMaster           = Id "mon_zombie_master"
monZombieNecro            = Id "mon_zombie_necro"
monZombieNecroBoomer      = Id "mon_zombie_necro_boomer"
monZombieRunner           = Id "mon_zombie_runner"
monZombieRegenerating     = Id "mon_zombie_regenerating"
monZombiePredator         = Id "mon_zombie_predator"
monZombieScreecher        = Id "mon_zombie_screecher"
monZombieShady            = Id "mon_zombie_shady"
monShia                   = Id "mon_shia"
monZombieShrieker         = Id "mon_zombie_shrieker"
monZombieSkull            = Id "mon_zombie_skull"
monZombieSmoker           = Id "mon_zombie_smoker"
monSmokerBrute            = Id "mon_smoker_brute"
monZombieSwimmerBase      = Id "mon_zombie_swimmer_base"
monZombieSwimmer          = Id "mon_zombie_swimmer"
monZombieTechnician       = Id "mon_zombie_technician"
monZombieMiner            = Id "mon_zombie_miner"
monZombieThorny           = Id "mon_zombie_thorny"
monZombieReenactor        = Id "mon_zombie_reenactor"
monZeriatric              = Id "mon_zeriatric"
monZombiePrisoner         = Id "mon_zombie_prisoner"
monZombiePrisonerBrute    = Id "mon_zombie_prisoner_brute"
monZombiePrisonerFat      = Id "mon_zombie_prisoner_fat"
monZombiePrisonerTough    = Id "mon_zombie_prisoner_tough"
monCharredNightmare       = Id "mon_charred_nightmare"
monIrradiatedWanderer1    = Id "mon_irradiated_wanderer_1"
monIrradiatedWanderer2    = Id "mon_irradiated_wanderer_2"
monIrradiatedWanderer3    = Id "mon_irradiated_wanderer_3"
monIrradiatedWanderer4    = Id "mon_irradiated_wanderer_4"
monSkeleton               = Id "mon_skeleton"
monSkeletonBrute          = Id "mon_skeleton_brute"
monSkeletonElectric       = Id "mon_skeleton_electric"
monSkeletonHulk           = Id "mon_skeleton_hulk"
monZombieSoldier          = Id "mon_zombie_soldier"
monZombieSoldierBlackops1 = Id "mon_zombie_soldier_blackops_1"
monZombieSoldierBlackops2 = Id "mon_zombie_soldier_blackops_2"
monZombieSoldierAcid1     = Id "mon_zombie_soldier_acid_1"
monZombieSoldierAcid2     = Id "mon_zombie_soldier_acid_2"
monZombieKevlar1          = Id "mon_zombie_kevlar_1"
monZombieKevlar2          = Id "mon_zombie_kevlar_2"
monZombieMilitaryPilot    = Id "mon_zombie_military_pilot"
monZombieMilbasePersonnel = Id "mon_zombie_milbase_personnel"
monZombieSailor           = Id "mon_zombie_sailor"
monZombieOfficer          = Id "mon_zombie_officer"
monZombieMarine           = Id "mon_zombie_marine"
monZombieMarineUpgrade    = Id "mon_zombie_marine_upgrade"
monZombieFlamer           = Id "mon_zombie_flamer"
monZombieArmored          = Id "mon_zombie_armored"
monZombieBioOp            = Id "mon_zombie_bio_op"
monZombieBioOp2           = Id "mon_zombie_bio_op2"
monZombieSurvivor         = Id "mon_zombie_survivor"
monZombieSurvivorElite    = Id "mon_zombie_survivor_elite"

monZombieMedical :: Id
monZombieMedicalBrute :: Id
monZombieMedicalRegenerating :: Id
monSkeletonMedical :: Id
monZombieMedicalAcidic :: Id
monZombieMedicalPupa :: Id
monZombiePupaMedicalDecoy :: Id

monZombieMedical             = Id "mon_zombie_medical"
monZombieMedicalBrute        = Id "mon_zombie_medical_brute"
monZombieMedicalRegenerating = Id "mon_zombie_medical_regenerating"
monSkeletonMedical           = Id "mon_skeleton_medical"
monZombieMedicalAcidic       = Id "mon_zombie_medical_acidic"
monZombieMedicalPupa         = Id "mon_zombie_medical_pupa"
monZombiePupaMedicalDecoy    = Id "mon_zombie_pupa_medical_decoy"

monZombieCrawlerPupaDecoy :: Id
monZombieCrawlerPupa :: Id
monZombiePupaDecoy :: Id
monZombiePupa :: Id
monBrutePupaDecoy :: Id
monBrutePupa :: Id
monHulkPupaDecoy :: Id
monHulkPupa :: Id
monZombiePupaDecoyShady :: Id
monZombiePupaShady :: Id

monZombieCrawlerPupaDecoy = Id "mon_zombie_crawler_pupa_decoy"
monZombieCrawlerPupa      = Id "mon_zombie_crawler_pupa"
monZombiePupaDecoy        = Id "mon_zombie_pupa_decoy"
monZombiePupa             = Id "mon_zombie_pupa"
monBrutePupaDecoy         = Id "mon_brute_pupa_decoy"
monBrutePupa              = Id "mon_brute_pupa"
monHulkPupaDecoy          = Id "mon_hulk_pupa_decoy"
monHulkPupa               = Id "mon_hulk_pupa"
monZombiePupaDecoyShady   = Id "mon_zombie_pupa_decoy_shady"
monZombiePupaShady        = Id "mon_zombie_pupa_shady"

monBeekeeper :: Id
monZombie :: Id
monZombieCop :: Id
monZombieCrawler :: Id
monZombieDancer :: Id
monZombieFat :: Id
monZombieFireman :: Id
monZombieHazmat :: Id
monZombieRot :: Id
monZombieSwat :: Id
monZombieTough :: Id
monZombieResortDancer :: Id
monZombieResortBouncer :: Id
monZombieResortStaff :: Id

monBeekeeper           = Id "mon_beekeeper"
monZombie              = Id "mon_zombie"
monZombieCop           = Id "mon_zombie_cop"
monZombieCrawler       = Id "mon_zombie_crawler"
monZombieDancer        = Id "mon_zombie_dancer"
monZombieFat           = Id "mon_zombie_fat"
monZombieFireman       = Id "mon_zombie_fireman"
monZombieHazmat        = Id "mon_zombie_hazmat"
monZombieRot           = Id "mon_zombie_rot"
monZombieSwat          = Id "mon_zombie_swat"
monZombieTough         = Id "mon_zombie_tough"
monZombieResortDancer  = Id "mon_zombie_resort_dancer"
monZombieResortBouncer = Id "mon_zombie_resort_bouncer"
monZombieResortStaff   = Id "mon_zombie_resort_staff"

monZombieWinged :: Id
monZombieBruteWinged :: Id
monSpawnRaptor :: Id
monSpawnRaptorShady :: Id
monSpawnRaptorUnstable :: Id
monSpawnRaptorElectric :: Id

monZombieWinged        = Id "mon_zombie_winged"
monZombieBruteWinged   = Id "mon_zombie_brute_winged"
monSpawnRaptor         = Id "mon_spawn_raptor"
monSpawnRaptorShady    = Id "mon_spawn_raptor_shady"
monSpawnRaptorUnstable = Id "mon_spawn_raptor_unstable"
monSpawnRaptorElectric = Id "mon_spawn_raptor_electric"

monShoggoth :: Id
monShoggoth = Id "mon_shoggoth"

monZhark :: Id
monZombieDog :: Id
monDogZombieCop :: Id
monDogZombieRot :: Id
monZolf :: Id
monZombear :: Id
monZombiePig :: Id
monZombeaver :: Id
monZoose :: Id
monZougar :: Id
monZombieHorse :: Id
monZiger :: Id
monZow :: Id
monZombull :: Id
monZllama :: Id
monZeer :: Id
monZpiderMass :: Id
monZeindeer :: Id
monGastroBufo :: Id

monZhark        = Id "mon_zhark"
monZombieDog    = Id "mon_zombie_dog"
monDogZombieCop = Id "mon_dog_zombie_cop"
monDogZombieRot = Id "mon_dog_zombie_rot"
monZolf         = Id "mon_zolf"
monZombear      = Id "mon_zombear"
monZombiePig    = Id "mon_zombie_pig"
monZombeaver    = Id "mon_zombeaver"
monZoose        = Id "mon_zoose"
monZougar       = Id "mon_zougar"
monZombieHorse  = Id "mon_zombie_horse"
monZiger        = Id "mon_ziger"
monZow          = Id "mon_zow"
monZombull      = Id "mon_zombull"
monZllama       = Id "mon_zllama"
monZeer         = Id "mon_zeer"
monZpiderMass   = Id "mon_zpider_mass"
monZeindeer     = Id "mon_zeindeer"
monGastroBufo   = Id "mon_gastro_bufo"
