{-# LANGUAGE OverloadedStrings #-}
module Cdda.Main where

import System.FilePath ((</>))
import qualified System.FilePath as F
import qualified System.Directory as D

import Data.Maybe
import qualified Data.Containers.ListUtils as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M

import Control.Lens
import Control.Applicative ( (<|>) )
import Data.Aeson
import Define.Core
import qualified Define.Json as J
import Define.Monster
import Define.Spell
import Define.MakeFields

import qualified Cdda.Id.Monster as I
import Cdda.Id.Harvest

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
import Cdda.Harvest
import Cdda.Monster.Strength

makeModInfo :: J.ModInfo
makeModInfo = J.ModInfo
  { J._modinfoCddaType     = "MOD_INFO"
  , J._modinfoId           = Id "zombie_tamer"
  , J._modinfoName         = "ゾンビテイマー"
  , J._modinfoAuthors      = [ "tlshtivo053153" ]
  , J._modinfoDescription  = "ゾンビと友達になることができます。"
  , J._modinfoCategory     = "content"
  , J._modinfoDependencies = [ "dda" ]
  , J._modinfoVersion      = "0.0.1"
  }

makeCddaMod :: J.CddaMod
makeCddaMod = J.CddaMod
  { J._cddaModModInfo        = (FP.getModInfo, [makeModInfo])
  , J._cddaModItemFood       = (FP.getItemFood, map J.convItem allPetfood)
  , J._cddaModMonsterVanilla =
      let f :: Monster -> J.Monster
          f m = J.Monster
            { J._monsterCopyFrom = m ^. base
            , J._monsterId = m ^. base
            , J._monsterCddaType = "MONSTER"
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
            , J._monsterPetfood        = Just $ J.convPetfood $ m ^. petfood
            , J._monsterChatTopics = Just $ return $ mergeId (m ^. base) (Id "MAIN")
            , J._monsterHarvest        =
                let z = idHarvestZombie <$> M.lookup (m^.base) allZombieMap
                    s = idHarvestSkeleton <$> M.lookup (m^.base) allSkeletonMap
                 in z <|> s
            }
          vMon monId = J.Monster
            { J._monsterCopyFrom = monId
            , J._monsterId = monId
            , J._monsterCddaType = "MONSTER"
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
            , J._monsterChatTopics = Nothing
            , J._monsterHarvest        =
                let z = idHarvestZombie <$> M.lookup monId allZombieMap
                    s = idHarvestSkeleton <$> M.lookup monId allSkeletonMap
                 in z <|> s
            }
          g monId = maybe (vMon monId) f $ getMonsterFriend monId
       in [(FP.getMonsterVanilla, map g I.allMonster)]
  , J._cddaModMonsterFriend  =
      let f i = do
            m <- J.convMonsters <$> getMonsterFriend i
            return (FP.getMonsterFriend i, m)
       in mapMaybe f I.allFriendMonster
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
    let spell = mapMaybe (fmap convSpell' . S.spellUpgradeRandom) allUpgradeRandomType
        convSpell' s = let s' = J.convSpell s in s' { J._spellFlags = "POLYMORPH_GROUP" : J._spellFlags s' }
     in [(FP.getSpellUpgradeRandom, spell)]
  , J._cddaModSpellUpgradeStandard =
    let spell = map (J.convSpell . S.spellUpgradeStandard) allUpgradeStandardList
        spellNub = L.nubOrdOn J._spellId spell
     in [(FP.getSpellUpgradeStandard, spellNub)]
  , J._cddaModUpgradeRandom  = [(FP.getUpgradeRandom, map J.convMonsterGroup allMonsterGroup)]
  , J._cddaModHarvest = (FP.getHarvest, map J.convHarvest allHarvest)
  }

outputCddaMod :: J.CddaMod -> IO ()
outputCddaMod m = mapM_ cddaJsonToFile $
  [ f $ m ^. modInfo ]
  ++ [ f $ m ^. itemFood ]
  ++ map f (m ^. monsterVanilla)
  ++ map f (m ^. monsterFriend)
  ++ map f (m ^. talkVanilla)
  ++ map f (m ^. talkFriend)
  ++ map f (m ^. spellToFriend)
  ++ map f (m ^. spellLevelUp)
  ++ map f (m ^. spellUpgradeRandom)
  ++ map f (m ^. spellUpgradeStandard)
  ++ map f (m ^. upgradeRandom)
  ++ [ f (m ^. harvest) ]
    where
      f (path, objs) = (path, encode objs)

cddaJsonToFile :: (FilePath, BL.ByteString) -> IO ()
cddaJsonToFile (relPath, objs) = do
  let path = "mods"</>"zombie_tamer"</> relPath
  D.createDirectoryIfMissing True $ F.takeDirectory path
  BL.writeFile path objs
