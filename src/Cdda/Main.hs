{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Cdda.Main where

import Prelude hiding (id)
import System.FilePath ((</>))
import qualified System.FilePath as F
import qualified System.Directory as D

import Data.Maybe
import qualified Data.Containers.ListUtils as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Bifunctor

import Control.Lens
import Control.Applicative ( (<|>) )
import Data.Aeson
import Define.Core
import qualified Define.Json as J
import Define.Monster
import Define.Spell
import Define.DeathFunction
import Define.Talk
import Define.MakeFields

import qualified Cdda.Id.Monster as I
import Cdda.Id.ItemGroup
import Cdda.Id.Harvest
import Cdda.Id.Spell

import qualified Cdda.FilePath as FP
import Cdda.Item
import qualified Cdda.Json as J
import Cdda.Monster
import Cdda.Monster.Upgrade
import Cdda.Talk.Vanilla
import Cdda.Talk.Friend
import Cdda.Talk.Utils
import qualified Cdda.Spell.Polymorph as S
import qualified Cdda.Spell.DeathFunction as S
import Cdda.MonsterGroup
import Cdda.Harvest
import Cdda.Monster.Strength
import Cdda.ItemGroup
import Cdda.HarvestDropType
import Cdda.Furniture
import Cdda.TerFurnTransform
import Cdda.DeathFunction

makeModInfo :: J.ModInfo
makeModInfo = J.ModInfo
  { J._modinfoCddaType     = "MOD_INFO"
  , J._modinfoId           = Id "zombie_tamer"
  , J._modinfoName         = "追加 - ゾンビテイマー"
  , J._modinfoAuthors      = [ "tlshtivo053153" ]
  , J._modinfoDescription  = T.concat
      [ "上級汚染肉、上級汚染骨髄などのアイテムを追加します。"
      , "そのアイテムを使ってゾンビと友達になることができます。"
      , "これらのアイテムは、精密解体などで入手できます。"
      ]
  , J._modinfoCategory     = "content"
  , J._modinfoDependencies = [ "dda" ]
  , J._modinfoVersion      = "0.0.2"
  }

makeCddaMod :: J.CddaMod
makeCddaMod = J.CddaMod
  { J._cddaModModInfo        = (FP.getModInfo, [makeModInfo])
  , J._cddaModItemFood       = (FP.getItemFood, map J.convItem allPetfood)
  , J._cddaModMonsterVanilla =
      let fMon :: Monster -> J.Monster
          fMon m = J.Monster
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
            , J._monsterHarvest        = Nothing
            , J._monsterDissect        =
              let zombie = idHarvestZombie <$> M.lookup (m ^. base) allZombieMap
                  skeleton = idHarvestSkeleton <$> M.lookup (m ^. base) allSkeletonMap
               in zombie <|> skeleton
            , J._monsterDeathFunction =
              let zombie = idSpellPlaceMeatSlime <$> M.lookup (m ^. base) allZombieMap
                  skeleton = idSpellPlaceMarrowSlime <$> M.lookup (m ^. base) allSkeletonMap
                  df = M.lookup (m ^. base) allDeathFunctionMap
                  df' = case df of
                          Just df_ -> df_ & id ?~ idSpellOverrideDeathFunction (m ^. base)
                          Nothing -> DeathFunction
                            { _deathFunctionId         = zombie <|> skeleton
                            , _deathFunctionHitSelf    = Just True
                            , _deathFunctionMinLevel   = Nothing
                            , _deathFunctionCorpseType = Nothing
                            , _deathFunctionMessage    = Nothing
                            }
               in Just $ J.convDeathFunction df'
            }
          nfMon :: Id -> J.Monster
          nfMon monId = J.Monster
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
            , J._monsterHarvest        = Nothing
            , J._monsterDissect        =
              let zombie = idHarvestZombie <$> M.lookup monId allZombieMap
                  skeleton = idHarvestSkeleton <$> M.lookup monId allSkeletonMap
               in zombie <|> skeleton
            , J._monsterDeathFunction =
              let zombie = idSpellPlaceMeatSlime <$> M.lookup monId allZombieMap
                  skeleton = idSpellPlaceMarrowSlime <$> M.lookup monId allSkeletonMap
                  df = M.lookup monId allDeathFunctionMap
                  df' = case df of
                          Just df_ -> df_ & id ?~ idSpellOverrideDeathFunction monId
                          Nothing -> DeathFunction
                            { _deathFunctionId         = zombie <|> skeleton
                            , _deathFunctionHitSelf    = Just True
                            , _deathFunctionMinLevel   = Nothing
                            , _deathFunctionCorpseType = Nothing
                            , _deathFunctionMessage    = Nothing
                            }
               in Just $ J.convDeathFunction df'
            }
          frineds = mapMaybe (fmap fMon . getMonsterFriend) I.allFriendMonster
          nonFriends = map nfMon I.allNonFriendMonster
      in [ (FP.getMonsterVanillaFriend, frineds)
         , (FP.getMonsterVanillaNonFriend, nonFriends)
         ]
  , J._cddaModMonsterFriend  =
      let f i = do
            m <- J.convMonsters <$> getMonsterFriend i
            return (FP.getMonsterFriend i, m)
       in mapMaybe f I.allFriendMonster
  , J._cddaModTalkVanilla    =
      let vanilla = concatMap (map J.convTalk . vanillaTalk) allMonsterFriend
       in [(FP.getTalkVanilla, vanilla)]
  , J._cddaModTalkFriend     =
      let f m = map (g m) $ friendTalk m
          g :: Monster -> (Int, [Talk]) -> (FilePath, [J.Talk])
          g m = bimap (FP.getTalkFriend (m ^. base))
                      (map J.convTalk)
       in concatMap f allMonsterFriend
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
  , J._cddaModSpellDeathFunc =
    let z = map S.spellPlaceMeatSlime allZombieStrength
        s = map S.spellPlaceMarrowSlime allSkeletonStrength
     in (FP.getSpellDeathFunc, map J.convSpellDeathFunc $ z ++ s)
  , J._cddaModSpellDeathFuncOverride =
    let dfs = mapMaybe f I.allMonster
        f monId = do
          df <- M.lookup monId allDeathFunctionMap
          dfId <- df ^. id
          let zombie = idSpellPlaceMeatSlime <$> M.lookup monId allZombieMap
              skeleton = idSpellPlaceMarrowSlime <$> M.lookup monId allSkeletonMap
          exId <- zombie <|> skeleton
          return $ S.spellDeathOverride monId dfId exId
     in (FP.getSpellDeathFuncOverride, map J.convSpellDeathFunctionOverride dfs)
  , J._cddaModUpgradeRandom  = [(FP.getUpgradeRandom, map J.convMonsterGroup allMonsterGroup)]
  , J._cddaModHarvest = (FP.getHarvest, map J.convHarvest allHarvest)
  , J._cddaModItemGroup = (FP.getItemGroup, map J.convItemGroup allItemGroup)
  , J._cddaModHarvestDropType = (FP.getHarvestDropType, [J.convHarvestDropType harvestDropTypeTaintedFood])
  , J._cddaModFurniture = (FP.getFurniture, map J.convFurniture allFurniture)
  , J._cddaModTerFurnTransform = (FP.getTerFurnTransform, map J.convTerFurnTransform allTerFurnTransform)
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
  ++ [ f (m ^. spellDeathFunc) ]
  ++ [ f (m ^. spellDeathFuncOverride) ]
  ++ map f (m ^. upgradeRandom)
  ++ [ f (m ^. harvest) ]
  ++ [ f (m ^. itemGroup) ]
  ++ [ f (m ^. harvestDropType) ]
  ++ [ f (m ^. furniture) ]
  ++ [ f (m ^. terFurnTransform) ]
    where
      f (path, objs) = (path, encode objs)

cddaJsonToFile :: (FilePath, BL.ByteString) -> IO ()
cddaJsonToFile (relPath, objs) = do
  let path = "mods"</>"zombie_tamer"</> relPath
  D.createDirectoryIfMissing True $ F.takeDirectory path
  BL.writeFile path objs
