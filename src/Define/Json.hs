{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Define.Json where

import GHC.Generics

import Define.Aeson
import qualified Define.Core as C
import qualified Define.Item as C
import qualified Define.Monster as C
import qualified Define.MonsterGroup as C
import qualified Define.Talk as CT

import Data.Aeson
import qualified Data.Text as T

type CddaJson a = (FilePath, [a])

data CddaMod = CddaMod
  { _cddaModModInfo :: CddaJson ModInfo
  , _cddaModItemFood :: CddaJson Item
  , _cddaModMonsterVanilla :: [CddaJson Monster]
  , _cddaModMonsterFriend :: [CddaJson Monster]
  , _cddaModTalkVanilla :: [CddaJson Talk]
  , _cddaModTalkFriend :: [CddaJson Talk]
  , _cddaModSpellToFriend :: [CddaJson Spell]
  , _cddaModSpellLevelUp :: [CddaJson Spell]
  , _cddaModSpellUpgradeRandom :: [CddaJson Spell]
  , _cddaModSpellUpgradeStandard :: [CddaJson Spell]
  , _cddaModUpgradeRandom :: [CddaJson MonsterGroup]
  , _cddaModHarvest :: CddaJson Harvest
  }

data ModInfo = ModInfo
  { _modinfoCddaType :: T.Text
  , _modinfoId :: C.Id
  , _modinfoName :: T.Text
  , _modinfoAuthors :: [T.Text]
  , _modinfoDescription :: T.Text
  , _modinfoCategory :: T.Text
  , _modinfoDependencies :: [T.Text]
  , _modinfoVersion :: T.Text
  }
  deriving Generic

instance ToJSON ModInfo where
  toJSON = genericToJSON cddaOption

data Item = Item
  { _itemCopyFrom    :: C.Id
  , _itemCddaType    :: T.Text
  , _itemId          :: C.Id
  , _itemName        :: C.Name
  , _itemDescription :: C.Description
  , _itemUseAction   :: [C.UseAction]
  , _itemPetfood     :: [C.FoodCategory]
  }
  deriving Generic

instance ToJSON Item where
  toJSON = genericToJSON cddaOption

data Monster = Monster
  { _monsterCopyFrom       :: C.Id
  , _monsterId             :: C.Id
  , _monsterCddaType       :: T.Text
  , _monsterHp             :: Maybe Int
  , _monsterSpeed          :: Maybe Int
  , _monsterDodge          :: Maybe Int
  , _monsterMeleeSkill     :: Maybe Int
  , _monsterMeleeDice      :: Maybe Int
  , _monsterMeleeDiceSides :: Maybe Int
  , _monsterMeleeDamage    :: Maybe [Damage]
  , _monsterArmorBash      :: Maybe Int
  , _monsterArmorBullet    :: Maybe Int
  , _monsterArmorCut       :: Maybe Int
  , _monsterArmorStab      :: Maybe Int
  , _monsterArmorAcid      :: Maybe Int
  , _monsterArmorFire      :: Maybe Int
  , _monsterArmorElec      :: Maybe Int
  , _monsterArmorCold      :: Maybe Int
  , _monsterArmorPure      :: Maybe Int
  , _monsterRegenerates    :: Maybe Int
  , _monsterPetfood        :: Maybe Petfood
  , _monsterChatTopics     :: Maybe [C.Id]
  }
  deriving Generic

instance ToJSON Monster where
  toJSON = genericToJSON cddaOption

data Petfood = Petfood
  { _petfoodFood :: [T.Text]
  , _petfoodFeed :: Maybe T.Text
  , _petfoodPet :: Maybe T.Text
  }
  deriving Generic

instance ToJSON Petfood where
  toJSON = genericToJSON cddaOption

data Damage = Damage
  { _damageDamageType :: T.Text
  , _damageAmount :: Int
  , _damageArmorPenetration :: Maybe Int
  }
  deriving Generic

instance ToJSON Damage where
  toJSON = genericToJSON cddaOption

data Talk = Talk
  { _talkId :: C.Id
  , _talkCddaType :: T.Text
  , _talkSpeakerEffect :: Maybe CT.Effect
  , _talkDynamicLine :: CT.DynamicLine
  , _talkResponses :: [Response]
  }
  deriving Generic

instance ToJSON Talk where
  toJSON = genericToJSON cddaOption

data Response = Response
  { _responseText :: T.Text
  , _responseCondition :: Maybe CT.Condition
  , _responseTrial :: Trial
  , _responseSuccess :: TrialResponse
  , _responseFailure :: Maybe TrialResponse
  }
  deriving Generic

instance ToJSON Response where
  toJSON = genericToJSON cddaOption

data Trial = Trial
  { _trialCddaType :: T.Text
  , _trialCondition :: Maybe CT.Condition
  }
  deriving Generic

instance ToJSON Trial where
  toJSON = genericToJSON cddaOption

data TrialResponse = TrialResponse
  { _trialresponseTopic :: T.Text
  , _trialresponseEffect :: Maybe TalkEffects
  }
  deriving Generic

instance ToJSON TrialResponse where
  toJSON = genericToJSON cddaOption

newtype TalkEffects = TalkEffects [CT.Effect]

instance ToJSON TalkEffects where
  toJSON (TalkEffects es) = toJSON es

data Spell = Spell
  { _spellId :: C.Id
  , _spellCddaType :: T.Text
  , _spellName :: C.Name
  , _spellDescription :: C.Description
  , _spellValidTargets :: [T.Text]
  , _spellEffect :: T.Text
  , _spellMinDamage :: Int
  , _spellMaxDamage :: Int
  , _spellMinRange :: Int
  , _spellFlags :: [T.Text]
  , _spellShape :: T.Text
  , _spellEffectStr :: C.Id
  }
  deriving Generic

instance ToJSON Spell where
  toJSON = genericToJSON cddaOption

data MonsterGroup = MonsterGroup
  { _monstergroupName :: C.Id
  , _monstergroupCddaType :: T.Text
  , _monstergroupMonsters :: [C.MGMonster]
  }
  deriving Generic

instance ToJSON MonsterGroup where
  toJSON = genericToJSON cddaOption

data Harvest = Harvest
  { _harvestId :: C.Id
  , _harvestCddaType :: T.Text
  , _harvestCopyFrom :: Maybe C.Id
  , _harvestEntries :: Maybe [Entry]
  }
  deriving Generic

instance ToJSON Harvest where
  toJSON = genericToJSON cddaOption

data Entry = Entry
  { _entryDrop :: C.Id
  , _entryCddaType :: T.Text
  , _entryMassRatio :: Maybe Double
  , _entryBaseNum :: Maybe [Int]
  , _entryScaleNum :: Maybe [Double]
  }
  deriving Generic

instance ToJSON Entry where
  toJSON = genericToJSON cddaOption
