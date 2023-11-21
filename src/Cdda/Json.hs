{-# LANGUAGE OverloadedStrings #-}
module Cdda.Json where

import Prelude hiding (id, pure)
import Control.Lens
import Define.MakeFields

import Cdda.Monster.Status
import Cdda.Id.Friend
import Cdda.Talk.Friend
import Cdda.Talk.Utils

import qualified Define.Json as J

import Define.Core
import Define.Item
import Define.Monster
import Define.Talk
import Define.Spell
import Define.MonsterGroup

convItem :: Item -> J.Item
convItem i = J.Item
  { J._itemCopyFrom    = i ^. copyFrom
  , J._itemCddaType    = "GENERIC"
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
  , J._spellValidTargets = []
  , J._spellEffect       = "noise"
  , J._spellMinDamage    = 100000000
  , J._spellMaxDamage    = 100000000
  , J._spellMinRange     = 1
  , J._spellFlags        = [ "NO_EXPLOSION_SFX" ]
  , J._spellShape        = "blast"
  , J._spellEffectStr    = s ^. effectStr
  }

convMonsterGroup :: MonsterGroup -> J.MonsterGroup
convMonsterGroup (MonsterGroup mgId mons) = J.MonsterGroup
  { J._monstergroupName     = mgId
  , J._monstergroupCddaType = "monstergroup"
  , J._monstergroupMonsters = mons
  }
