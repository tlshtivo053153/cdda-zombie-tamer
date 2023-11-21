{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Define.Talk where

import GHC.Generics (Generic)

import Data.Text (Text)
import qualified Data.Vector as V

import Data.Aeson
import Data.Aeson.Casing

import Define.Aeson
import Define.Core
import Define.Monster

newtype Op = Op Text
  deriving Generic

instance ToJSON Op

data Val = UValVar Text
         | NpcValVar Text

data Arithmetic = ArithmeticAssign Val Arithmetic
                | ArithmeticOperation Arithmetic Op Arithmetic
                | ArithmeticConst Int
                | ArithmeticRand Int
                | ArithmeticVal Val

instance ToJSON Arithmetic where
  toJSON a =
    let f x = object [ "arithmetic" .= x ]
     in case a of
               ArithmeticAssign (UValVar v) a' -> f
                [ object [ "u_val" .= ("var" :: Text)
                         , "var_name" .= v
                         ]
                , "="
                , toJSON a'
                ]
               ArithmeticAssign (NpcValVar v) a' -> f
                [ object [ "npc_val" .= ("var" :: Text)
                         , "var_name" .= v
                         ]
                , "="
                , toJSON a'
                ]
               ArithmeticOperation a1 op a2 -> f
                [ toJSON a1
                , toJSON op
                , toJSON a2
                ]
               ArithmeticConst n ->
                 object [ "const" .= n ]
               ArithmeticRand n ->
                 object [ "rand" .= n ]
               ArithmeticVal (UValVar v) ->
                 object [ "u_val" .= ("var" :: Text)
                        , "var_name" .= v
                        ]
               ArithmeticVal (NpcValVar v) ->
                 object [ "npc_val" .= ("var" :: Text)
                        , "var_name" .= v
                        ]

data Var = Var Text Text Text

data DynamicLine
    = DynamicLineCompare CompareVar DynamicLine DynamicLine
    | DynamicLineText Text

instance ToJSON DynamicLine where
  toJSON d =
    case d of
      DynamicLineCompare cv d1 d2 ->
        case cv of
          UCompareVar (Var v t c) op i -> object
            [ "u_compare_var" .= v
            , "type" .= t
            , "context" .= c
            , "op" .= op
            , "value" .= i
            , "yes" .= d1
            , "no" .= d2
            ]
          NpcCompareVar (Var v t c) op i -> object
            [ "npc_compare_var" .= v
            , "type" .= t
            , "context" .= c
            , "op" .= op
            , "value" .= i
            , "yes" .= d1
            , "no" .= d2
            ]
          UCompareTime (Var v t c) op time -> object
            [ "u_compare_time_since_var" .= v
            , "type" .= t
            , "context" .= c
            , "op" .= op
            , "time" .= time
            , "yes" .= d1
            , "no" .= d2
            ]
          NpcCompareTime (Var v t c) op time -> object
            [ "u_compare_time_since_var" .= v
            , "type" .= t
            , "context" .= c
            , "op" .= op
            , "time" .= time
            , "yes" .= d1
            , "no" .= d2
            ]
      DynamicLineText t -> toJSON t

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

instance ToJSON Condition where
  toJSON c = case toJSONCondition c of
               Just c' -> c'
               Nothing -> toJSON (Nothing :: Maybe Text)

toJSONCondition :: Condition -> Maybe Value
toJSONCondition (ConditionAnd cs)          = Just $ object [ "and" .= map toJSONCondition cs ]
toJSONCondition (ConditionOr cs)           = Just $ object [ "or" .= map toJSONCondition cs ]
toJSONCondition (ConditionNot c)           = Just $ object [ "not" .= toJSONCondition c ]
toJSONCondition (UHasItems itemId n)       = Just $ object [ "u_has_items" .= object
                                                      [ "item" .= itemId
                                                      , "count" .= n
                                                      ]
                                                    ]
toJSONCondition ConditionNone              = Nothing
toJSONCondition (ConditionCompareVar cvar) =
  let toCompareVarObject comp (Var v t c) op n =
          Just $ object [ comp .= v
                 , "type" .= t
                 , "context" .= c
                 , "op" .= op
                 , "value" .= n
                 ]
      toCompareTimeObject comp (Var v t c) op time =
          Just $ object [ comp .= v
                 , "type" .= t
                 , "context" .= c
                 , "op" .= op
                 , "time" .= time
                 ]
  in case cvar of
    UCompareVar v op n -> toCompareVarObject "u_compare_var" v op n
    NpcCompareVar v op n -> toCompareVarObject "npc_compare_var" v op n
    UCompareTime v op time -> toCompareTimeObject "u_compare_time" v op time
    NpcCompareTime v op time -> toCompareTimeObject "npc_compare_time" v op time

data Effect = EffectArithmetic Arithmetic
            | NpcCastSpell Id Bool
            | UConsumeItem Id Int
            | UMessage Text Text Bool
            | UAdjustVar Var Int
            | NpcAdjustVar Var Int
            | UAddMorale Id Int Int Text Text
            | UAddEffect Id Int
            | NpcAddEffect Id Int

instance ToJSON Effect where
  toJSON (EffectArithmetic arith  ) = toJSON arith
  toJSON (NpcCastSpell spellId b  ) = object [ "npc_cast_spell" .=
                                                  object [ "id" .= spellId ]
                                             , "hit_self" .= b
                                             ]
  toJSON (UConsumeItem itemId n   ) = object [ "u_consume_item" .= itemId
                                             , "count" .= n
                                             ]
  toJSON (UMessage t1 t2 b        ) = object [ "u_message" .= t1
                                             , "type" .= t2
                                             , "popup" .= b
                                             ]
  toJSON (UAdjustVar (Var v t c) n          ) = object [ "u_adjust_var" .= v
                                                       , "type" .= t
                                                       , "context" .= c
                                                       , "adjustment" .= n
                                                       ]
  toJSON (NpcAdjustVar (Var v t c) n) = object [ "npc_adjust_var" .= v
                                               , "type" .= t
                                               , "context" .= c
                                               , "adjustment" .= n
                                               ]
  toJSON (UAddMorale mId bonus mbonus t1 t2) = object [ "u_add_morale" .= mId
                                                      , "bonus" .= bonus
                                                      , "max_bonus" .= mbonus
                                                      , "duration" .= t1
                                                      , "decay_start" .= t2
                                                      ]
  toJSON (UAddEffect effectId n   ) = object [ "u_add_effect" .= effectId
                                             , "duration" .= n
                                             ]
  toJSON (NpcAddEffect effectId n ) = object [ "npc_add_effect" .= effectId
                                             , "duration" .= n
                                             ]

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
