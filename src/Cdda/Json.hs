{-# LANGUAGE OverloadedStrings #-}
module Cdda.Json
  ( convItemPetfood
  , convMonsters
  , convDeathFunction
  , convPetfood
  , concatTalk
  , convSpellPolymorph
  , convSpellTerTransform
  , convSpellDeathFunctionOverride
  , convMonsterGroup
  , convHarvest
  , convEntry
  , convItemGroup
  , convHarvestDropType
  , convFurniture
  , convTerFurnTransform
  , convTransFurniture
  , convFlag
  ) where

import Prelude hiding (id, pure)
import Control.Applicative ( (<|>) )
import Control.Lens
import Define.MakeFields
import qualified Data.Map as M
import Data.Ratio
import Data.Maybe

import Cdda.Monster.Status
import Cdda.Id.Friend
import Cdda.Id.Harvest
import Cdda.Id.HarvestDropType
import Cdda.Id.Spell
import Cdda.Talk.Utils
import Cdda.Monster.Strength
import Cdda.DeathFunction
import Cdda.Flag.Level
import Cdda.Flag.Upgrade

import qualified Define.Json as J

import Define.Core
import Define.Item.Petfood
import Define.Monster
import Define.Talk
import Define.Spell
import Define.MonsterGroup
import Define.Harvest
import Define.ItemGroup
import Define.HarvestDropType
import Define.Furniture
import Define.TerFurnTransform
import Define.DeathFunction
import Define.EOC
import Define.Flag

convItemPetfood :: ItemPetfood -> J.Item
convItemPetfood i = J.Item
  { J._itemCopyFrom    = i ^. copyFrom
  , J._itemCddaType    = "COMESTIBLE"
  , J._itemId          = i ^. id
  , J._itemName        = i ^. name
  , J._itemDescription = i ^. description
  , J._itemUseAction   = i ^. useAction
  , J._itemPetfood     = i ^. petfood
  }

convMonsters :: Monster -> [J.Monster]
convMonsters m = map f statuss
  where
    maxLevel' = m ^. growth.maxLevel
    growth' = m ^. growth
    status' = m ^. status
    statuss = map (\l -> (,) l $ statusWithLevel l growth' status') [1..maxLevel']
    f :: (Int, Status) -> J.Monster
    f (l, monStatus) =
      let monMelee = monStatus ^. melee
          monArmor = monStatus ^. armor
          monId = monFriend (m ^. base) l
       in J.Monster
          { J._monsterCopyFrom       = m ^. base
          , J._monsterId             = monId
          , J._monsterCddaType       = "MONSTER"
          , J._monsterHp             = Just $ monStatus ^. hp
          , J._monsterSpeed          = Just $ monStatus ^. speed
          , J._monsterDodge          = Just $ monStatus ^. dodge
          , J._monsterMeleeSkill     = Just $ monMelee ^. skill
          , J._monsterMeleeDice      = Just $ monMelee ^. dice
          , J._monsterMeleeDiceSides = Just $ monMelee ^. diceSides
          , J._monsterMeleeDamage    = Just $ map convDamage $ monMelee ^. damage
          , J._monsterArmor          = Just $ J.Armor
              { J._armorBash     = Just $ monArmor ^. bash
              , J._armorBullet   = Just $ monArmor ^. bullet
              , J._armorCut      = Just $ monArmor ^. cut
              , J._armorStab     = Just $ monArmor ^. stab
              , J._armorAcid     = Just $ monArmor ^. acid
              , J._armorHeat     = Just $ monArmor ^. heat
              , J._armorElectric = Just $ monArmor ^. elec
              , J._armorCold     = Just $ monArmor ^. cold
              , J._armorPure     = Just $ monArmor ^. pure
              }
          , J._monsterRegenerates    = Just $ monStatus ^. regenerates
          , J._monsterPetfood        = Just $ convPetfood $ m ^. petfood
          , J._monsterChatTopics     = Just $ return $ mergeId monId (Id "MAIN")
          , J._monsterHarvest        = Nothing
          , J._monsterDissect        =
              let zombie = idHarvestZombie <$> M.lookup (m ^. base) allZombieMap
                  skeleton = idHarvestSkeleton <$> M.lookup (m ^. base) allSkeletonMap
               in zombie <|> skeleton
          , J._monsterDeathFunction =
              let zombie = idSpellPlaceMeatSlime <$> M.lookup (m ^. base) allZombieMap
                  skeleton = idSpellPlaceMarrowSlime <$> M.lookup (m ^. base) allSkeletonMap
                  addSpell = zombie <|> skeleton
                  df = M.lookup (m ^. base) allDeathFunctionMap
                  df' = case df of
                          Just df_ -> df_ & id ?~ idSpellOverrideDeathFunction (m ^. base)
                          Nothing -> DeathFunction
                            { _deathFunctionId         = addSpell
                            , _deathFunctionHitSelf    = Just True
                            , _deathFunctionMinLevel   = Nothing
                            , _deathFunctionCorpseType = Nothing
                            , _deathFunctionMessage    = Nothing
                            }
               in addSpell *> Just (convDeathFunction df')
          , J._monsterExtend = Just $ J.Extend
              { J._extendFlags =
                let flagLevel = runFlag $ getLevel l
                    flagRandoms = map runFlag $ getRandom $ m ^. base
                    flagStandards = map runFlag $ getStandard $ m ^. base
                 in [flagLevel] ++ flagRandoms ++ flagStandards
              }
          }

convDeathFunction :: DeathFunction -> J.DeathFunction
convDeathFunction df = J.DeathFunction
  { J._deathfunctionEffect     =
      case df ^. id of
       Just _ -> Just J.DeathFunctionEffect
                    { J._deathfunctioneffectId       = df ^. id
                    , J._deathfunctioneffectHitSelf  = df ^. hitSelf
                    , J._deathfunctioneffectMinLevel = df ^. minLevel
                    }
       Nothing -> Nothing
  , J._deathfunctionMessage    = df ^. message
  , J._deathfunctionCorpseType = df ^. corpseType
  }

convDamage :: Damage -> J.Damage
convDamage d = J.Damage
  { J._damageDamageType       = d ^. damageType
  , J._damageAmount           = d ^. amount
  , J._damageArmorPenetration = Just $ d ^. armorPenetration
  }

convPetfood :: PetFood -> J.Petfood
convPetfood (PetFood pcs) = J.Petfood
  { J._petfoodFood = pcs
  , J._petfoodFeed = Nothing
  , J._petfoodPet  = Nothing
  }

convTalk :: Id -> Id -> Talk -> Maybe J.Talk
convTalk topId preId t = do
  talkId' <- t ^? talkId
  dynamicLine' <- t ^? dynamicLine
  return $ J.Talk
    { J._talkId            = talkId'
    , J._talkCddaType      = "talk_topic"
    , J._talkSpeakerEffect = case t ^. speakerEffect of
                               [] -> Nothing
                               es -> Just $ J.SpeakerEffect
                                 { J._speakereffectSentinel  = Just $ runId talkId' <> "_sentinel"
                                 , J._speakereffectCondition = Nothing
                                 , J._speakereffectEffect    = es
                                 }
    , J._talkDynamicLine   = dynamicLine'
    , J._talkResponses     = map (convResponse topId preId) $ t ^. responses
    }

concatTalk :: Talk -> [J.Talk]
concatTalk t = case t ^? talkId of
                 Nothing -> []
                 Just topId ->
                       let t' = maybeToList (convTalk topId topId t)
                           ts = t^.responses & getResponsesHasTalk & concatMap (loop topId topId)
                        in t' ++ ts
  where
    loop :: Id -> Id -> Talk -> [J.Talk]
    loop top prev u@(Talk { _talkTalkId = tId}) =
      let ress = u ^. responses
          nextTalks = getResponsesHasTalk ress
          ts = concatMap (loop top tId) nextTalks
       in maybeToList (convTalk top prev u) ++ ts
    loop _ _ _ = []

getResponsesHasTalk :: [Response] -> [Talk]
getResponsesHasTalk = concatMap getResponseHasTalk

getResponseHasTalk :: Response -> [Talk]
getResponseHasTalk r = catMaybes [ Just $ r ^. success
                                 , r ^. failure
                                 ]

convResponse :: Id -> Id -> Response -> J.Response
convResponse topId preId r = J.Response
  { J._responseText      = r ^. text
  , J._responseCondition = case r ^. condition of
                             ConditionNone -> Nothing
                             _ -> Just $ r ^. condition
  , J._responseTrial     =
      case r ^. trial of
        ConditionNone -> J.Trial
          { J._trialCddaType  = "NONE"
          , J._trialCondition = Nothing
          }
        c -> J.Trial
          { J._trialCddaType  = "CONDITION"
          , J._trialCondition = Just c
          }
  , J._responseSuccess   = J.TrialResponse
      { J._trialresponseTopic = case r ^. success of
                                  TalkBack -> preId
                                  TalkTop -> topId
                                  TalkDone -> Id "TALK_DONE"
                                  Talk { _talkTalkId = t } -> t
      , J._trialresponseEffect = case r ^. successEffect of
                                   [] -> Nothing
                                   es -> Just $ convTalkEffects es
      }
  , J._responseFailure   = case r ^. trial of
                             ConditionNone -> Nothing
                             _ -> Just $ J.TrialResponse
                               { J._trialresponseTopic = case r ^. failure of
                                  Just TalkBack -> preId
                                  Just TalkTop -> topId
                                  Just TalkDone -> Id "TALK_DONE"
                                  Just (Talk {_talkTalkId = t }) -> t
                                  _ -> Id "TALK_DONE"
                               , J._trialresponseEffect = case r ^. failureEffect of
                                                            [] -> Nothing
                                                            es -> Just $ convTalkEffects es
                               }
  }

convTalkEffects :: [Effect] -> J.TalkEffects
convTalkEffects = J.TalkEffects

convSpellPolymorph :: SpellPolymorph -> J.Spell
convSpellPolymorph s = J.Spell
  { J._spellId           = s ^. id
  , J._spellCddaType     = "SPELL"
  , J._spellName         = s ^. name
  , J._spellDescription  = s ^. description
  , J._spellValidTargets = [ "ally" ]
  , J._spellEffect       = Just "targeted_polymorph"
  , J._spellMinDamage    = 100000000
  , J._spellMaxDamage    = 100000000
  , J._spellMinRange     = Just 1
  , J._spellFlags        = [ "NO_FAIL", "SILENT", "NO_EXPLOSION_SFX" ]
  , J._spellShape        = "blast"
  , J._spellEffectStr    = Just $ s ^. effectStr
  , J._spellMinAoe = Nothing
  , J._spellMaxAoe = Nothing
  , J._spellExtraEffects = Nothing
  }

convSpellTerTransform :: SpellTerTransform -> J.Spell
convSpellTerTransform s = J.Spell
  { J._spellId           = s ^. id
  , J._spellCddaType     = "SPELL"
  , J._spellName         = s ^. name
  , J._spellDescription  = s ^. description
  , J._spellValidTargets = [ "ground" ]
  , J._spellEffect       = Just "ter_transform"
  , J._spellMinDamage    = 0
  , J._spellMaxDamage    = 0
  , J._spellMinRange     = Nothing
  , J._spellFlags        = [ "NO_FAIL", "SILENT", "NO_EXPLOSION_SFX", "IGNORE_WALLS" ]
  , J._spellShape        = "blast"
  , J._spellEffectStr    = Just $ s ^. effectStr
  , J._spellMinAoe = Just 1
  , J._spellMaxAoe = Just 1
  , J._spellExtraEffects = Nothing
  }

convSpellDeathFunctionOverride :: SpellDeathFunctionOverride -> J.Spell
convSpellDeathFunctionOverride s = J.Spell
  { J._spellId           = s ^. id
  , J._spellCddaType     = "SPELL"
  , J._spellName         = s ^. name
  , J._spellDescription  = s ^. description
  , J._spellValidTargets = [ "ground", "ally" ]
  , J._spellEffect       = Just "noise"
  , J._spellMinDamage    = 0
  , J._spellMaxDamage    = 0
  , J._spellMinRange     = Nothing
  , J._spellFlags        = [ "NO_FAIL", "SILENT", "NO_EXPLOSION_SFX" ]
  , J._spellShape        = "blast"
  , J._spellEffectStr    = Nothing
  , J._spellMinAoe = Nothing
  , J._spellMaxAoe = Nothing
  , J._spellExtraEffects = Just $ map (\(Id i) -> J.SpellExtraEffect i) $ s ^. effects
  }

convMonsterGroup :: MonsterGroup -> J.MonsterGroup
convMonsterGroup (MonsterGroup mgId mons) = J.MonsterGroup
  { J._monstergroupName     = mgId
  , J._monstergroupCddaType = "monstergroup"
  , J._monstergroupMonsters = mons
  }

convHarvest :: Harvest -> J.Harvest
convHarvest (Harvest hId es) = J.Harvest
  { J._harvestId       = hId
  , J._harvestCddaType = "harvest"
  , J._harvestCopyFrom = Nothing
  , J._harvestEntries  = Just $ map convEntry es
  }

convEntry :: Entry -> J.Entry
convEntry (EntryRatio eId hType massRatio) = J.Entry
  { J._entryDrop      = eId
  , J._entryCddaType  = case hType of
                          Flesh -> "flesh"
                          Blood -> "blood"
                          Bone -> "bone"
                          TaintedFood -> case idHarvestDropTypeTaintedFood of
                                           Id i -> i
  , J._entryMassRatio = Just massRatio
  , J._entryBaseNum   = Nothing
  , J._entryScaleNum  = Nothing
  }
convEntry (EntryBase eId hType (baseMin, baseMax) (scaleMin, scaleMax)) = J.Entry
  { J._entryDrop      = eId
  , J._entryCddaType  = case hType of
                          Flesh -> "flesh"
                          Blood -> "blood"
                          Bone -> "bone"
                          TaintedFood -> case idHarvestDropTypeTaintedFood of
                                           Id i -> i
  , J._entryMassRatio = Nothing
  , J._entryBaseNum   = Just [baseMin, baseMax]
  , J._entryScaleNum  = Just [scaleMin, scaleMax]
  }
convEntry (EntryDrop eId hType) = J.Entry
  { J._entryDrop      = eId
  , J._entryCddaType  = case hType of
                          Flesh -> "flesh"
                          Blood -> "blood"
                          Bone -> "bone"
                          TaintedFood -> case idHarvestDropTypeTaintedFood of
                                           Id i -> i
  , J._entryMassRatio = Nothing
  , J._entryBaseNum   = Nothing
  , J._entryScaleNum  = Nothing
  }

convItemGroup :: ItemGroup -> J.ItemGroup
convItemGroup ig = J.ItemGroup
  { J._itemgroupCddaType = "item_group"
  , J._itemgroupId       = ig ^. id
  , J._itemgroupSubtype  = "distribution"
  , J._itemgroupEntries  = map convItemEntry es'
  }
    where
      es = ig ^. entries
      probLcm = map (\e -> e ^. prob & denominator) es
                & foldl lcm 1
                & fromIntegral
      es' :: [(Id, Integer)]
      es' = map (\e -> (e^.item, numerator $ e^.prob * probLcm)) es

convItemEntry :: (Id, Integer) -> J.ItemEntry
convItemEntry (eId, eProb) = J.ItemEntry
  { J._itementryItem = eId
  , J._itementryProb = fromIntegral eProb
  }

convHarvestDropType :: HarvestDropType -> J.HarvestDropType
convHarvestDropType hdt = J.HarvestDropType
  { J._harvestdroptypeCddaType = "harvest_drop_type"
  , J._harvestdroptypeId       = hdt ^. id
  , J._harvestdroptypeGroup    = hdt ^. group
  }

convFurniture :: Furniture -> J.Furniture
convFurniture f = J.Furniture
  { J._furnitureCddaType    = "furniture"
  , J._furnitureId          = f ^. id
  , J._furnitureName        = f ^. name
  , J._furnitureDescription = f ^. description
  , J._furnitureSymbol      = "s"
  , J._furnitureColor       = "dark_gray"
  , J._furnitureMoveCostMod = 1
  , J._furnitureRequiredStr = 0
  , J._furnitureFlags       = [ "TRANSPARENT"
                              , "PLACE_ITEM"
                              , "NOCOLLIDE"
                              ]
  , J._furnitureBash        = J.FurnitureBash
    { J._furniturebashStrMin    = 0
    , J._furniturebashStrMax    = 0
    , J._furniturebashSoundVol  = 1
    , J._furniturebashSound     = "ビチャ"
    , J._furniturebashSoundFail = "ボヨン"
    , J._furniturebashItems     =
      let i = J.FurnitureItem $ f ^. itemGroup
       in [i]
    }
  , J._furnitureLooksLike = Just "f_alien_pod_organ"
  }

convTerFurnTransform :: TerFurnTransform -> J.TerFurnTransform
convTerFurnTransform t = J.TerFurnTransform
  { J._terfurntransformCddaType  = "ter_furn_transform"
  , J._terfurntransformId        = t ^. id
  , J._terfurntransformFurniture = map convTransFurniture $ t ^. furniture
  }

convTransFurniture :: TransFurniture -> J.TransFurniture
convTransFurniture t = J.TransFurniture
  { J._transfurnitureResult         = t ^. result
  , J._transfurnitureValidFurniture = [t ^. validFurniture]
  }

convFlag :: Flag -> J.Flag
convFlag (Flag fId) = J.Flag
  { J._flagType = "monster_flag"
  , J._flagId = Id fId
  }
