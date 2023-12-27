{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Cdda.DeathFunction
  ( allDeathFunctionMap
  ) where

import Define.Core
import Define.DeathFunction
import Define.MakeFields

import Cdda.Id.Spell
import Cdda.Id.Monster

import qualified Data.Text as T
import qualified Data.Map as M
import Control.Lens
import Prelude hiding (id)

noCorpse :: T.Text
noCorpse = "NO_CORPSE"

defDeathFunction :: DeathFunction
defDeathFunction = DeathFunction
  { _deathFunctionId         = Nothing
  , _deathFunctionHitSelf    = Nothing
  , _deathFunctionMinLevel   = Nothing
  , _deathFunctionCorpseType = Nothing
  , _deathFunctionMessage    = Nothing
  }

deathGasZombie :: DeathFunction
deathGasZombie = defDeathFunction
  & id ?~ idSpellDeathGasZombie
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "A %s explodes!"

gasZombieList :: [Id]
gasZombieList = [ monGasZombie ]

deathChildZombie :: DeathFunction
deathChildZombie = defDeathFunction
  & id ?~ idSpellDeathChildZombie
  & minLevel ?~ 6

childZombieList :: [Id]
childZombieList =
  [ monZombieChild
  , monZombieWaif
  ]

deathSmokerZombie :: DeathFunction
deathSmokerZombie = defDeathFunction
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
deathZombieHollow = defDeathFunction
  & id ?~ idSpellDeathZombieHollow
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "The %s splits in two!"

zombieHollowList :: [Id]
zombieHollowList = [ monZombieHollow ]

deathNecroBoomer :: DeathFunction
deathNecroBoomer = defDeathFunction
  & id ?~ idSpellDeathNecroBoomer
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "The %s lets out an unholy screech, and the corpses around it stir."

necroBoomerList :: [Id]
necroBoomerList =
  [ monZombieNecroBoomer ]

deathZombieGasbag :: DeathFunction
deathZombieGasbag = defDeathFunction
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
deathBoomer = defDeathFunction
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
deathBoomerHuge = defDeathFunction
  & id ?~ idSpellDeathBoomerHuge
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "A %s explodes!"

boomerHugeList :: [Id]
boomerHugeList = [ monBoomerHuge ]

deathZombieRelaxGasbag :: DeathFunction
deathZombieRelaxGasbag = defDeathFunction
  & id ?~ idSpellDeathZombieRelaxGasbag
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "A %s explodes!"

zombieRelaxGasbagList :: [Id]
zombieRelaxGasbagList = [ monZombieRelaxGasbag ]

deathZombieTearGasbag :: DeathFunction
deathZombieTearGasbag = defDeathFunction
  & id ?~ idSpellDeathZombieTearGasbag
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "A %s explodes!"

zombieTearGasbagList :: [Id]
zombieTearGasbagList = [ monZombieTearGasbag ]

deathDevourerLabSec :: DeathFunction
deathDevourerLabSec = defDeathFunction
  & id ?~ idSpellDeathDevourerLabSec
  & hitSelf ?~ True
  & minLevel ?~ 1
  & corpseType ?~ noCorpse
  & message ?~ "The creature grows sluggish before freezing in place for a moment.  Suddenly its flesh turns black and recedes to coat the central figure, which reaches out towards you before collapsing into a pinprick of shimmering air.  Torn uniforms and other gear tumble to the ground."

devourerLabSecList :: [Id]
devourerLabSecList = [ monDevourerLabSec ]

deathZombieChildScorched :: DeathFunction
deathZombieChildScorched = defDeathFunction
  & id ?~ idSpellDeathZombieChildScorched
  & minLevel ?~ 4
  & message ?~ "A %s explodes!"

zombieChildScorchedList :: [Id]
zombieChildScorchedList = [ monZombieChildScorched ]

deathZombieAcidic :: DeathFunction
deathZombieAcidic = defDeathFunction
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
deathSpawnRaptorUnstable = defDeathFunction
  & id ?~ idSpellDeathSpawnRaptorUnstable
  & hitSelf ?~ True
  & corpseType ?~ noCorpse
  & message ?~ "The %s explodes!"

spawnRaptorUnstableList :: [Id]
spawnRaptorUnstableList = [ monSpawnRaptorUnstable ]


deathShoggoth :: DeathFunction
deathShoggoth = defDeathFunction
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
