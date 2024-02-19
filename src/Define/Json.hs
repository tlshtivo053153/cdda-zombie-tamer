{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Define.Json
  ( CddaJson
  , CddaMod(..)
  , ModInfo(..)
  , Item(..)
  , Monster(..)
  , Armor(..)
  , Petfood(..)
  , Damage(..)
  , DeathFunction(..)
  , DeathFunctionEffect(..)
  , Extend(..)
  , Talk(..)
  , SpeakerEffect(..)
  , Response(..)
  , Trial(..)
  , TrialResponse(..)
  , TalkEffects(..)
  , Spell(..)
  , SpellExtraEffect(..)
  , MonsterGroup(..)
  , Harvest(..)
  , Entry(..)
  , ItemGroup(..)
  , ItemEntry(..)
  , HarvestDropType(..)
  , Furniture(..)
  , FurnitureBash(..)
  , FurnitureItem(..)
  , TerFurnTransform(..)
  , TransFurniture(..)
  , Flag(..)
  , Eoc(..)
  ) where

import GHC.Generics

import Define.Aeson
import qualified Define.Core as C
import qualified Define.MonsterGroup as C
import qualified Define.Talk as CT
import qualified Define.EOC as E

import Data.Aeson
import qualified Data.Text as T

type CddaJson a = (FilePath, [a])

data CddaMod = CddaMod
  { _cddaModModInfo :: CddaJson ModInfo
  , _cddaModItemFood :: CddaJson Item
  , _cddaModMonsterVanilla :: [CddaJson Monster]
  , _cddaModMonsterFriend :: [CddaJson Monster]
  , _cddaModTalkVanilla :: [CddaJson Talk]
  , _cddaModTalkFriend :: CddaJson Talk
  , _cddaModSpellToFriend :: [CddaJson Spell]
  , _cddaModSpellLevelUp :: [CddaJson Spell]
  , _cddaModSpellUpgradeRandom :: [CddaJson Spell]
  , _cddaModSpellUpgradeStandard :: [CddaJson Spell]
  , _cddaModSpellDeathFunc :: CddaJson Spell
  , _cddaModSpellDeathFuncOverride :: CddaJson Spell
  , _cddaModUpgradeRandom :: [CddaJson MonsterGroup]
  , _cddaModFriendGroup :: CddaJson MonsterGroup
  , _cddaModHarvest :: CddaJson Harvest
  , _cddaModItemGroup :: CddaJson ItemGroup
  , _cddaModHarvestDropType :: CddaJson HarvestDropType
  , _cddaModFurniture :: CddaJson Furniture
  , _cddaModTerFurnTransform :: CddaJson TerFurnTransform
  , _cddaModFlag :: CddaJson Flag
  , _cddaModEoc :: [CddaJson Eoc]
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
  , _monsterArmor      :: Maybe Armor
  , _monsterRegenerates    :: Maybe Int
  , _monsterPetfood        :: Maybe Petfood
  , _monsterChatTopics     :: Maybe [C.Id]
  , _monsterHarvest        :: Maybe C.Id
  , _monsterDissect        :: Maybe C.Id
  , _monsterDeathFunction  :: Maybe DeathFunction
  , _monsterExtend         :: Maybe Extend
  }
  deriving Generic

instance ToJSON Monster where
  toJSON = genericToJSON cddaOption

data Armor = Armor
  { _armorBash     :: Maybe Int
  , _armorBullet   :: Maybe Int
  , _armorCut      :: Maybe Int
  , _armorStab     :: Maybe Int
  , _armorAcid     :: Maybe Int
  , _armorHeat     :: Maybe Int
  , _armorElectric :: Maybe Int
  , _armorCold     :: Maybe Int
  , _armorPure     :: Maybe Int
  }
  deriving Generic

instance ToJSON Armor where
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

data DeathFunction = DeathFunction
  { _deathfunctionEffect :: Maybe DeathFunctionEffect
  , _deathfunctionMessage :: Maybe T.Text
  , _deathfunctionCorpseType :: Maybe T.Text
  }
  deriving Generic

instance ToJSON DeathFunction where
  toJSON = genericToJSON cddaOption

data DeathFunctionEffect = DeathFunctionEffect
  { _deathfunctioneffectId :: Maybe C.Id
  , _deathfunctioneffectHitSelf :: Maybe Bool
  , _deathfunctioneffectMinLevel :: Maybe Int
  }
  deriving Generic

instance ToJSON DeathFunctionEffect where
  toJSON = genericToJSON cddaOption

newtype Extend = Extend
  { _extendFlags :: [T.Text]
  }
  deriving Generic

instance ToJSON Extend where
  toJSON = genericToJSON cddaOption

data Talk = Talk
  { _talkId :: C.Id
  , _talkCddaType :: T.Text
  , _talkSpeakerEffect :: Maybe SpeakerEffect
  , _talkDynamicLine :: CT.DynamicLine
  , _talkResponses :: [Response]
  }
  deriving Generic

instance ToJSON Talk where
  toJSON = genericToJSON cddaOption

data SpeakerEffect = SpeakerEffect
  { _speakereffectSentinel :: Maybe T.Text
  , _speakereffectCondition :: Maybe E.Condition
  , _speakereffectEffect :: [E.Effect]
  }
  deriving Generic

instance ToJSON SpeakerEffect where
  toJSON = genericToJSON cddaOption

data Response = Response
  { _responseText :: T.Text
  , _responseCondition :: Maybe E.Condition
  , _responseTrial :: Trial
  , _responseSuccess :: TrialResponse
  , _responseFailure :: Maybe TrialResponse
  }
  deriving Generic

instance ToJSON Response where
  toJSON = genericToJSON cddaOption

data Trial = Trial
  { _trialCddaType :: T.Text
  , _trialCondition :: Maybe E.Condition
  }
  deriving Generic

instance ToJSON Trial where
  toJSON = genericToJSON cddaOption

data TrialResponse = TrialResponse
  { _trialresponseTopic :: C.Id
  , _trialresponseEffect :: Maybe TalkEffects
  }
  deriving Generic

instance ToJSON TrialResponse where
  toJSON = genericToJSON cddaOption

newtype TalkEffects = TalkEffects [E.Effect]

instance ToJSON TalkEffects where
  toJSON (TalkEffects es) = toJSON es

data Spell = Spell
  { _spellId :: C.Id
  , _spellCddaType :: T.Text
  , _spellName :: C.Name
  , _spellDescription :: C.Description
  , _spellValidTargets :: [T.Text]
  , _spellEffect :: Maybe T.Text
  , _spellMinDamage :: Int
  , _spellMaxDamage :: Int
  , _spellMinRange :: Maybe Int
  , _spellFlags :: [T.Text]
  , _spellShape :: T.Text
  , _spellEffectStr :: Maybe C.Id
  , _spellMinAoe :: Maybe Int
  , _spellMaxAoe :: Maybe Int
  , _spellExtraEffects :: Maybe [SpellExtraEffect]
  }
  deriving Generic

instance ToJSON Spell where
  toJSON = genericToJSON cddaOption

newtype SpellExtraEffect = SpellExtraEffect T.Text

instance ToJSON SpellExtraEffect where
  toJSON (SpellExtraEffect e) = object [ "id" .= e, "hit_self" .= True ]

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

data ItemGroup = ItemGroup
  { _itemgroupCddaType :: T.Text
  , _itemgroupId :: C.Id
  , _itemgroupSubtype :: T.Text
  , _itemgroupEntries :: [ItemEntry]
  }
  deriving Generic

instance ToJSON ItemGroup where
  toJSON = genericToJSON cddaOption

data ItemEntry = ItemEntry
  { _itementryItem :: C.Id
  , _itementryProb :: Int
  }
  deriving Generic

instance ToJSON ItemEntry where
  toJSON = genericToJSON cddaOption

data HarvestDropType = HarvestDropType
  { _harvestdroptypeCddaType :: T.Text
  , _harvestdroptypeId :: C.Id
  , _harvestdroptypeGroup :: Bool
  }
  deriving Generic

instance ToJSON HarvestDropType where
  toJSON = genericToJSON cddaOption

data Furniture = Furniture
  { _furnitureCddaType :: T.Text
  , _furnitureId :: C.Id
  , _furnitureName :: T.Text
  , _furnitureDescription :: T.Text
  , _furnitureSymbol :: T.Text
  , _furnitureColor :: T.Text
  , _furnitureMoveCostMod :: Int
  , _furnitureRequiredStr :: Int
  , _furnitureFlags :: [T.Text]
  , _furnitureBash :: FurnitureBash
  , _furnitureLooksLike :: Maybe T.Text
  }
  deriving Generic

instance ToJSON Furniture where
  toJSON = genericToJSON cddaOption

data FurnitureBash = FurnitureBash
  { _furniturebashStrMin :: Int
  , _furniturebashStrMax :: Int
  , _furniturebashSoundVol :: Int
  , _furniturebashSound :: T.Text
  , _furniturebashSoundFail :: T.Text
  , _furniturebashItems :: [FurnitureItem]
  }
  deriving Generic

instance ToJSON FurnitureBash where
  toJSON = genericToJSON cddaOption

newtype FurnitureItem = FurnitureItem
  { _furnitrueitemGroup :: C.Id }
  deriving Generic

instance ToJSON FurnitureItem where
  toJSON = genericToJSON cddaOption

data TerFurnTransform = TerFurnTransform
  { _terfurntransformCddaType :: T.Text
  , _terfurntransformId :: C.Id
  , _terfurntransformFurniture :: [TransFurniture]
  }
  deriving Generic

instance ToJSON TerFurnTransform where
  toJSON = genericToJSON cddaOption

data TransFurniture = TransFurniture
  { _transfurnitureResult :: [(C.Id, Int)]
  , _transfurnitureValidFurniture :: [C.Id]
  }
  deriving Generic

instance ToJSON TransFurniture where
  toJSON = genericToJSON cddaOption

data Flag = Flag
  { _flagType :: T.Text
  , _flagId :: C.Id
  }
  deriving Generic

instance ToJSON Flag where
  toJSON = genericToJSON cddaOption

data Eoc = Eoc
  { _eocType :: T.Text
  , _eocId :: C.Id
  , _eocEffect :: [E.Effect]
  }
  deriving Generic

instance ToJSON Eoc where
  toJSON = genericToJSON cddaOption
