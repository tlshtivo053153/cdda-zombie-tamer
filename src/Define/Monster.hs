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
  , Strength(..)
  , PetFood(..)
  , UpgradeRandomType(..)
  , UpgradeCondition(..)
  , UpgradeRandom(..)
  , UpgradeStandard(..)
  , Monster(..)
  ) where

import Data.Text

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

data Damage = Damage
  { _damageDamageType :: Text
  , _damageAmount :: Int
  , _damageArmorPenetration :: Int
  }

data Melee = Melee
  { _meleeSkill     :: Int
  , _meleeDice      :: Int
  , _meleeDiceSides :: Int
  , _meleeDamage    :: [Damage]
  }

data Status = Status
  { _statusHp :: Int
  , _statusSpeed :: Int
  , _statusDodge :: Int
  , _statusArmor :: Armor
  , _statusMelee :: Melee
  , _statusRegenerates :: Int
  }

-- Growth a b -> STATUS: stats * a^(level-1) + b*(level-1)
data Growth = Growth Rational Rational

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

data DamageGrowth = DamageGrowth
  { _damageGrowthDamageType :: Text
  , _damageGrowthAmount :: Growth
  , _damageGrowthArmorPenetration :: Growth
  }

data MeleeGrowth = MeleeGrowth
  { _meleeGrowthSkill :: Growth
  , _meleeGrowthDice :: Growth
  , _meleeGrowthDiceSides :: Growth
  , _meleeGrowthDamage :: [DamageGrowth]
  }

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

newtype Strength = Strength Int

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

