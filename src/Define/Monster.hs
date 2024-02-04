{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Define.Monster
  ( Armor(..)
  , Damage(..)
  , Melee(..)
  , Status(..)
  , Growth(..)
  , ArmorGrowth(..)
  , DamageGrowth(..)
  , MeleeGrowth(..)
  , StatusGrowth(..)
  , Strength
  , PetFood(..)
  , UpgradeRandomType(..)
  , UpgradeCondition(..)
  , UpgradeRandom(..)
  , UpgradeStandard(..)
  , Monster(..)
  ) where

import GHC.Generics (Generic)
import Data.Text
import Data.Default

import Define.Core

data Armor = Armor
  { _armorBash   :: Int
  , _armorBullet :: Int
  , _armorCut    :: Int
  , _armorStab   :: Int
  , _armorAcid   :: Int
  , _armorHeat   :: Int
  , _armorElec   :: Int
  , _armorCold   :: Int
  , _armorPure   :: Int
  }
  deriving (Generic, Default)

data Damage = Damage
  { _damageDamageType :: Text
  , _damageAmount :: Int
  , _damageArmorPenetration :: Int
  }
  deriving (Generic, Default)

data Melee = Melee
  { _meleeSkill     :: Int
  , _meleeDice      :: Int
  , _meleeDiceSides :: Int
  , _meleeDamage    :: [Damage]
  }
  deriving (Generic, Default)

data Status = Status
  { _statusHp :: Int
  , _statusSpeed :: Int
  , _statusDodge :: Int
  , _statusArmor :: Armor
  , _statusMelee :: Melee
  , _statusRegenerates :: Int
  }
  deriving (Generic, Default)

-- Growth a b -> STATUS: stats * a^(level-1) + b*(level-1)
data Growth = Growth Rational Rational

instance Default Growth where
  def = Growth 1 0

data ArmorGrowth = ArmorGrowth
  { _armorGrowthBash :: Growth
  , _armorGrowthBullet :: Growth
  , _armorGrowthCut :: Growth
  , _armorGrowthStab :: Growth
  , _armorGrowthAcid :: Growth
  , _armorGrowthHeat :: Growth
  , _armorGrowthElec :: Growth
  , _armorGrowthCold :: Growth
  , _armorGrowthPure :: Growth
  }
  deriving (Generic, Default)

data DamageGrowth = DamageGrowth
  { _damageGrowthDamageType :: Text
  , _damageGrowthAmount :: Growth
  , _damageGrowthArmorPenetration :: Growth
  }
  deriving (Generic, Default)

data MeleeGrowth = MeleeGrowth
  { _meleeGrowthSkill :: Growth
  , _meleeGrowthDice :: Growth
  , _meleeGrowthDiceSides :: Growth
  , _meleeGrowthDamage :: [DamageGrowth]
  }
  deriving (Generic, Default)

data StatusGrowth = StatusGrowth
  { _statusGrowthHp :: Growth
  , _statusGrowthSpeed :: Growth
  , _statusGrowthDodge :: Growth
  , _statusGrowthArmor :: ArmorGrowth
  , _statusGrowthMelee :: MeleeGrowth
  , _statusGrowthRegenerates :: Growth
  , _statusGrowthExp :: Growth
  , _statusGrowthMaxLevel :: Int
  }
  deriving (Generic, Default)

type Strength = Int

newtype PetFood = PetFood [FoodCategory]

data UpgradeRandomType
  = URNormal
  | URFat
  | URMedical
  | URBoomer
  | URRust
  | URLabsecurity
  | URElectric
  | URSkeleton
  | URNone
  deriving Show

data UpgradeCondition
  = UCTrue
  | UCFalse
  | UCHaveItem Id Int

data UpgradeRandom = UpgradeRandom UpgradeCondition UpgradeRandomType

data UpgradeStandard = UpgradeStandard UpgradeCondition Id

data Monster = Monster
  { _monsterBase :: Id
  , _monsterStatus :: Status
  , _monsterGrowth :: StatusGrowth
  , _monsterStrength :: Strength
  , _monsterPetfood :: PetFood
  , _monsterFriendCost :: Int
  , _monsterUpgradeRandom :: UpgradeRandom
  , _monsterUpgradeStandard :: [UpgradeStandard]
  }

