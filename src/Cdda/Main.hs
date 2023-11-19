{-# LANGUAGE OverloadedStrings #-}
module Cdda.Main where

import qualified System.FilePath as F
import qualified System.Directory as D

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BL

import Control.Lens
import Data.Aeson
import Define.Core
import qualified Define.Json as J
import Define.Monster
import Define.Spell
import Define.MakeFields

import qualified Cdda.Id.Monster as I

import qualified Cdda.FilePath as FP
import Cdda.Item
import qualified Cdda.Json as J
import Cdda.Monster
import Cdda.Monster.Upgrade
import Cdda.Talk.Vanilla
import Cdda.Talk.Friend
import Cdda.Talk.Utils
import qualified Cdda.Spell as S
import Cdda.MonsterGroup

makeCddaMod :: J.CddaMod
makeCddaMod = J.CddaMod
  { J._cddaModItemFood       = (FP.getItemFood, map J.convItem allPetfood)
  , J._cddaModMonsterVanilla =
      let f :: Monster -> J.Monster
          f m = J.Monster
            { J._monsterCopyFrom = m ^. base
            , J._monsterId = m ^. base
            , J._monsterCddaType = ""
            , J._monsterHp             = Nothing
            , J._monsterSpeed          = Nothing
            , J._monsterDodge          = Nothing
            , J._monsterMeleeSkill     = Nothing
            , J._monsterMeleeDice      = Nothing
            , J._monsterMeleeDiceSides = Nothing
            , J._monsterMeleeDamage    = Nothing
            , J._monsterArmorBash      = Nothing
            , J._monsterArmorBullet    = Nothing
            , J._monsterArmorCut       = Nothing
            , J._monsterArmorStab      = Nothing
            , J._monsterArmorAcid      = Nothing
            , J._monsterArmorFire      = Nothing
            , J._monsterArmorElec      = Nothing
            , J._monsterArmorCold      = Nothing
            , J._monsterArmorPure      = Nothing
            , J._monsterRegenerates    = Nothing
            , J._monsterPetfood        = Nothing
            , J._monsterChatTopics = Just $ return $ mergeId (m ^. base) (Id "MAIN")
            }
       in [(FP.getMonsterVanilla, map f allMonsterFriend)]
  , J._cddaModMonsterFriend  =
      let f i = do
            m <- J.convMonsters <$> getMonsterFriend i
            return (FP.getMonsterFriend i, m)
       in mapMaybe f I.allMonster
  , J._cddaModTalkVanilla    =
      let vanilla = concatMap (map J.convTalk . vanillaTalk) allMonsterFriend
       in [(FP.getTalkVanilla, vanilla)]
  , J._cddaModTalkFriend     =
      let f :: Monster -> FilePath
          f m = FP.getTalkFriend $ m ^. base
          g m = map J.convTalk $ friendTalk m
       in map (\m -> (f m, g m)) allMonsterFriend
  , J._cddaModSpellToFriend  =
      let spell = map (\m -> J.convSpell $ S.spellToFriend $ m ^. base) allMonsterFriend
       in [(FP.getSpellToFriend, spell)]
  , J._cddaModSpellLevelUp   =
    let f :: Monster -> FilePath
        f m = FP.getSpellLevelUp $ m ^. base
        g :: Monster -> [J.Spell]
        g m = map (J.convSpell . S.spellLevelUp (m ^. base)) [2.. m ^. growth.maxLevel]
     in map (\m -> (f m, g m)) allMonsterFriend
  , J._cddaModSpellUpgradeRandom =
    let spell = mapMaybe (fmap J.convSpell . S.spellUpgradeRandom) allUpgradeRandomType
     in [(FP.getSpellUpgradeRandom, spell)]
  , J._cddaModSpellUpgradeStandard =
    let spell = map (J.convSpell . S.spellUpgradeStandard) allUpgradeStandardList
     in [(FP.getSpellUpgradeStandard, spell)]
  , J._cddaModUpgradeRandom  = [(FP.getUpgradeRandom, map J.convMonsterGroup allMonsterGroup)]
  }

outputCddaMod :: J.CddaMod -> IO ()
outputCddaMod m = mapM_ cddaJsonToFile $
  [ f $ m ^. itemFood ]
  ++ map f (m ^. monsterVanilla)
  ++ map f (m ^. monsterFriend)
  ++ map f (m ^. talkVanilla)
  ++ map f (m ^. talkFriend)
  ++ map f (m ^. spellToFriend)
  ++ map f (m ^. spellLevelUp)
  ++ map f (m ^. upgradeRandom)
    where
      f (path, objs) = (path, encode objs)

cddaJsonToFile :: (FilePath, BL.ByteString) -> IO ()
cddaJsonToFile (relPath, objs) = do
  let path = "mods" F.</> relPath
  D.createDirectoryIfMissing True $ F.takeDirectory path
  BL.writeFile path objs
