{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Cdda.DeathFunction
  ( allDeathFunctionMap
  ) where

import Define.Core
import Define.DeathFunction
import Define.MakeFields

import Cdda.Id.Spell
import Cdda.Id.Monster

import Data.Default
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Lens
import Prelude hiding (id)

noCorpse :: T.Text
noCorpse = "NO_CORPSE"

deathGasZombie :: DeathFunction
deathGasZombie = def
  & id ?~ idSpellDeathGasZombie
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "A %s explodes!"

gasZombieList :: [Id]
gasZombieList = [ monGasZombie ]

deathChildZombie :: DeathFunction
deathChildZombie = def
  & id ?~ idSpellDeathChildZombie
  & minLevel ?~ 6

childZombieList :: [Id]
childZombieList =
  [ monZombieChild
  , monZombieWaif
  ]

deathSmokerZombie :: DeathFunction
deathSmokerZombie = def
  & id ?~ idSpellDeathSmokerZombie
  & hitSelf ?~ True
  & message ?~ "A %s explode!"

smokerZombieList :: [Id]
smokerZombieList =
  [ monZombieScorched
  , monZombieFiend
  , monSmokerBrute
  , monZombieSmoker
  ]

deathZombieHollow :: DeathFunction
deathZombieHollow = def
  & id ?~ idSpellDeathZombieHollow
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "The %s splits in two!"

zombieHollowList :: [Id]
zombieHollowList = [ monZombieHollow ]

deathNecroBoomer :: DeathFunction
deathNecroBoomer = def
  & id ?~ idSpellDeathNecroBoomer
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "The %s lets out an unholy screech, and the corpses around it stir."

necroBoomerList :: [Id]
necroBoomerList =
  [ monZombieNecroBoomer ]

deathZombieGasbag :: DeathFunction
deathZombieGasbag = def
  & id ?~ idSpellDeathZombieGasbag
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "A %s explodes!"

zombieGasbagList :: [Id]
zombieGasbagList =
  [ monZombieGasbag
  , monZombieHangingInnards
  , monZombieScissorlimbs
  , monZombieGasbagImpaler
  , monZombieGasbagCrawler
  , monZombieGasbagImmobile
  , monZombieCrushedGiant
  ]

deathBoomer :: DeathFunction
deathBoomer = def
  & id ?~ idSpellDeathBoomer
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "A %s explodes!"

boomerList :: [Id]
boomerList =
  [ monBoomer
  , monBoomerGlutton
  ]

deathBoomerHuge :: DeathFunction
deathBoomerHuge = def
  & id ?~ idSpellDeathBoomerHuge
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "A %s explodes!"

boomerHugeList :: [Id]
boomerHugeList = [ monBoomerHuge ]

deathZombieRelaxGasbag :: DeathFunction
deathZombieRelaxGasbag = def
  & id ?~ idSpellDeathZombieRelaxGasbag
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "A %s explodes!"

zombieRelaxGasbagList :: [Id]
zombieRelaxGasbagList = [ monZombieRelaxGasbag ]

deathZombieTearGasbag :: DeathFunction
deathZombieTearGasbag = def
  & id ?~ idSpellDeathZombieTearGasbag
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "A %s explodes!"

zombieTearGasbagList :: [Id]
zombieTearGasbagList = [ monZombieTearGasbag ]

deathDevourerLabSec :: DeathFunction
deathDevourerLabSec = def
  & id ?~ idSpellDeathDevourerLabSec
  & hitSelf ?~ True
  & minLevel ?~ 1
  & corpseType ?~ noCorpse
  & message ?~ "The creature grows sluggish before freezing in place for a moment.  Suddenly its flesh turns black and recedes to coat the central figure, which reaches out towards you before collapsing into a pinprick of shimmering air.  Torn uniforms and other gear tumble to the ground."

devourerLabSecList :: [Id]
devourerLabSecList = [ monDevourerLabSec ]

deathZombieChildScorched :: DeathFunction
deathZombieChildScorched = def
  & id ?~ idSpellDeathZombieChildScorched
  & minLevel ?~ 4
  & message ?~ "A %s explodes!"

zombieChildScorchedList :: [Id]
zombieChildScorchedList = [ monZombieChildScorched ]

deathZombieAcidic :: DeathFunction
deathZombieAcidic = def
  & id ?~ idSpellDeathZombieAcidic
  & hitSelf ?~ True
  & message ?~ "The %s's body leaks acid."

zombieAcidicList :: [Id]
zombieAcidicList =
  [ monZombieWretched
  , monZombieSpitter
  , monZombieCorrosive
  , monZombieAcidic
  , monZombieSoldierAcid1
  , monZombieSoldierAcid2
  , monTadpoleGrabber
  ]

deathSpawnRaptorUnstable :: DeathFunction
deathSpawnRaptorUnstable = def
  & id ?~ idSpellDeathSpawnRaptorUnstable
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "The %s explodes!"

spawnRaptorUnstableList :: [Id]
spawnRaptorUnstableList = [ monSpawnRaptorUnstable ]


deathShoggoth :: DeathFunction
deathShoggoth = def
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "The %s melts away."

shoggothList :: [Id]
shoggothList = [ monShoggoth ]

allDeathFunctionMap :: M.Map Id DeathFunction
allDeathFunctionMap = M.fromList $ concatMap toPair
  [ (,) gasZombieList deathGasZombie
  , (,) childZombieList deathChildZombie
  , (,) smokerZombieList deathSmokerZombie
  , (,) zombieHollowList deathZombieHollow
  , (,) necroBoomerList deathNecroBoomer
  , (,) zombieGasbagList deathGasZombie
  , (,) boomerList deathBoomer
  , (,) boomerHugeList deathBoomerHuge
  , (,) zombieRelaxGasbagList deathZombieRelaxGasbag
  , (,) zombieTearGasbagList deathZombieTearGasbag
  , (,) devourerLabSecList deathDevourerLabSec
  , (,) zombieChildScorchedList deathZombieChildScorched
  , (,) zombieAcidicList deathZombieAcidic
  , (,) spawnRaptorUnstableList deathSpawnRaptorUnstable
  , (,) shoggothList deathShoggoth
  ]
  where
    toPair (mons, df) = map (,df) mons
