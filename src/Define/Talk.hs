module Define.Talk where

import Data.Text
import qualified Data.Vector as V

import Define.Core
import Define.Monster

newtype Op = Op Text

data Val = UValVar Text
         | NpcValVar Text

data Arithmetic = ArithmeticAssign Val Arithmetic
                | ArithmeticOperation Arithmetic Op Arithmetic
                | ArithmeticConst Int
                | ArithmeticRand Int
                | ArithmeticVal Val

data Var = Var Text Text Text

data DynamicLine
    = DynamicLineCompare CompareVar DynamicLine DynamicLine
    | DynamicLineText Text

data CompareVar = UCompareVar Var Op Int
                | NpcCompareVar Var Op Int
                | UCompareTime Var Op Text
                | NpcCompareTime Var Op Text

data Condition = ConditionAnd [Condition]
               | ConditionOr [Condition]
               | ConditionNot Condition
               | UHasItems Id Int
               | ConditionCompareVar CompareVar
               | ConditionNone

data Effect = EffectArithmetic Arithmetic
            | NpcCastSpell Id Bool
            | UConsumeItem Id Int
            | UMessage Text Text Bool
            | UAdjustVar Var Int
            | NpcAdjustVar Var Int
            | UAddMorale Id Int Int Text Text
            | UAddEffect Id Int
            | NpcAddEffect Id Int

data TResponse = TResponse
  { _tResponseTopic :: Id
  , _tResponseEffect :: [Effect]
  }

data Trial = Trial Condition TResponse TResponse

data Response = Response
  { _responseText :: Text
  , _responseTrial :: Trial
  , _responseCondition :: Condition
  }

data Talk = Talk
  { _talkId :: Id
  , _talkSpeakerEffect :: Maybe Effect
  , _talkDynamicLine :: DynamicLine
  , _talkResponses :: [Response]
  }

data TalkConfig = TalkConfig
  { _talkConfigMonsterId :: Id
  , _talkConfigUpgradeRandom :: UpgradeRandom
  , _talkConfigUpgradeStandard :: [UpgradeStandard]
  , _talkConfigPetfood :: PetFood
  , _talkConfigStatus :: Status
  , _talkConfigLevel :: Int
  , _talkConfigNeedExp :: V.Vector Int
  }
