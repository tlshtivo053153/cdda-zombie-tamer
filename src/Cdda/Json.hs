{-# LANGUAGE OverloadedStrings #-}
module Cdda.Json where

import Prelude hiding (id, pure)
import Control.Applicative ( (<|>) )
import Control.Lens
import Define.MakeFields
import qualified Data.Map as M
import Data.Ratio

import Cdda.Monster.Status
import Cdda.Id.Friend
import Cdda.Id.Harvest
import Cdda.Id.HarvestDropType
import Cdda.Id.ItemGroup
import Cdda.Id.Spell
import Cdda.Talk.Friend
import Cdda.Talk.Utils
import Cdda.Monster.Strength
import Cdda.HarvestDropType
import Cdda.Furniture
import Cdda.TerFurnTransform

import qualified Define.Json as J

import Define.Core
import Define.Item
import Define.Monster
import Define.Talk
import Define.Spell
import Define.MonsterGroup
import Define.Harvest
import Define.ItemGroup
import Define.HarvestDropType
import Define.Furniture
import Define.TerFurnTransform

convItem :: Item -> J.Item
convItem i = J.Item
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
          , J._monsterArmorBash      = Just $ monArmor ^. bash
          , J._monsterArmorBullet    = Just $ monArmor ^. bullet
          , J._monsterArmorCut       = Just $ monArmor ^. cut
          , J._monsterArmorStab      = Just $ monArmor ^. stab
          , J._monsterArmorAcid      = Just $ monArmor ^. acid
          , J._monsterArmorFire      = Just $ monArmor ^. fire
          , J._monsterArmorElec      = Just $ monArmor ^. elec
          , J._monsterArmorCold      = Just $ monArmor ^. cold
          , J._monsterArmorPure      = Just $ monArmor ^. pure
          , J._monsterRegenerates    = Just $ monStatus ^. regenerates
          , J._monsterPetfood        = Just $ convPetfood $ m ^. petfood
          , J._monsterChatTopics     = Just $ return $ mergeId monId (Id "MAIN")
          , J._monsterHarvest        = Nothing
          , J._monsterDissect        =
              let zombie = idHarvestZombie <$> M.lookup (m ^. base) allZombieMap
                  skeleton = idHarvestSkeleton <$> M.lookup (m ^. base) allSkeletonMap
               in zombie <|> skeleton
          , J._monsterDeathFunction =
              let zombie = idSpellPlaceMeatSlime <$> M.lookup monId allZombieMap
                  skeleton = idSpellPlaceMarrowSlime <$> M.lookup monId allSkeletonMap
               in fmap J.DeathFunction $ zombie <|> skeleton
          }

convDamage :: Damage -> J.Damage
convDamage d = J.Damage
  { J._damageDamageType       = d ^. damageType
  , J._damageAmount           = d ^. amount
  , J._damageArmorPenetration = Just $ d ^. armorPenetration
  }

convPetfood :: PetFood -> J.Petfood
convPetfood (PetFood pcs) = J.Petfood
  { J._petfoodFood = pcs'
  , J._petfoodFeed = Nothing
  , J._petfoodPet  = Nothing
  }
    where
      pcs' = map (\(FoodCategory t) -> t) pcs

convTalk :: Talk -> J.Talk
convTalk t = J.Talk
  { J._talkId            = t ^. id
  , J._talkCddaType      = "talk_topic"
  , J._talkSpeakerEffect = t ^. speakerEffect
  , J._talkDynamicLine   = t ^. dynamicLine
  , J._talkResponses     = map convResponse $ t ^. responses
  }

convResponse :: Response -> J.Response
convResponse r = J.Response
  { J._responseText      = r ^. text
  , J._responseCondition = case r ^. condition of
                             ConditionNone -> Nothing
                             _ -> Just $ r ^. condition
  , J._responseTrial     =
      case cond of
        ConditionNone -> J.Trial
          { J._trialCddaType  = "NONE"
          , J._trialCondition = Nothing
          }
        _ -> J.Trial
          { J._trialCddaType  = "CONDITION"
          , J._trialCondition = Just cond
          }
  , J._responseSuccess   = convTrialResponse rs
  , J._responseFailure   = case cond of
                             ConditionNone -> Nothing
                             _ -> Just $ convTrialResponse rf
  }
    where
      (cond, rs, rf) = case r ^. trial of
                         (Trial c r1 r2) -> (c, r1, r2)

convTrialResponse :: TResponse -> J.TrialResponse
convTrialResponse tr = J.TrialResponse
  { J._trialresponseTopic  = case tr ^. topic of
                               Id i -> i
  , J._trialresponseEffect = case tr ^. effect of
                               [] -> Nothing
                               es -> Just $ convTalkEffects es
  }

convTalkEffects :: [Effect] -> J.TalkEffects
convTalkEffects = J.TalkEffects

convSpell :: Spell -> J.Spell
convSpell s = J.Spell
  { J._spellId           = s ^. id
  , J._spellCddaType     = "SPELL"
  , J._spellName         = Name $ s ^. name
  , J._spellDescription  = Description $ s ^. description
  , J._spellValidTargets = [ "ally" ]
  , J._spellEffect       = "targeted_polymorph"
  , J._spellMinDamage    = 100000000
  , J._spellMaxDamage    = 100000000
  , J._spellMinRange     = Just 1
  , J._spellFlags        = [ "NO_FAIL", "SILENT", "NO_EXPLOSION_SFX" ]
  , J._spellShape        = "blast"
  , J._spellEffectStr    = s ^. effectStr
  , J._spellMinAoe = Nothing
  , J._spellMaxAoe = Nothing
  }

convSpellDeathFunc :: Spell -> J.Spell
convSpellDeathFunc s = J.Spell
  { J._spellId           = s ^. id
  , J._spellCddaType     = "SPELL"
  , J._spellName         = Name $ s ^. name
  , J._spellDescription  = Description $ s ^. description
  , J._spellValidTargets = [ "ground" ]
  , J._spellEffect       = "ter_transform"
  , J._spellMinDamage    = 0
  , J._spellMaxDamage    = 0
  , J._spellMinRange     = Nothing
  , J._spellFlags        = [ "NO_FAIL", "SILENT", "NO_EXPLOSION_SFX", "IGNORE_WALLS" ]
  , J._spellShape        = "blast"
  , J._spellEffectStr    = s ^. effectStr
  , J._spellMinAoe = Just 1
  , J._spellMaxAoe = Just 1
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
  { J._furnitrueCddaType    = "furniture"
  , J._furnitrueId          = f ^. id
  , J._furnitrueName        = f ^. name
  , J._furnitrueDescription = f ^. description
  , J._furnitrueSymbol      = "s"
  , J._furnitrueColor       = "dark_gray"
  , J._furnitrueMoveCostMod = 1
  , J._furnitrueRequiredStr = 0
  , J._furnitrueFlags       = [ "TRANSPARENT"
                              , "PLACE_ITEM"
                              , "NOCOLLIDE"
                              ]
  , J._furnitrueBash        = J.FurnitureBash
    { J._furniturebashStrMin    = 0
    , J._furniturebashStrMax    = 0
    , J._furniturebashSoundVol  = 1
    , J._furniturebashSound     = "ビチャ"
    , J._furniturebashSoundFail = "ボヨン"
    , J._furniturebashItems     =
      let i = J.FurnitureItem $ f ^. itemGroup
       in [i]
    }
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
