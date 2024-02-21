{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Define.EOC
  ( Eoc(..)
  , Val(..)
  , Math(..)
  , MathExpr(..)
  , MathOp(..)
  , Op(..)
  , Arithmetic(..)
  , Var(..)
  , CompareVar(..)
  , Condition(..)
  , EValue(..)
  , Effect(..)
  , Expr(..)
  , ToEValue(..)
  ) where

import GHC.Generics (Generic)

import Data.Default
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Define.Core

data Eoc = Eoc
  { _eocId :: Id
  , _eocRecurrence :: Maybe Int
  , _eocCondition :: Maybe Condition
  , _eocDeactiveConditon :: Maybe Condition
  , _eocEffect :: [Effect]
  , _eocFalseEffect :: Maybe [Effect]
  }

instance Default Eoc where
  def = Eoc
    { _eocId = Id ""
    , _eocRecurrence = Nothing
    , _eocCondition = Nothing
    , _eocDeactiveConditon = Nothing
    , _eocEffect = []
    , _eocFalseEffect = Nothing
    }

data Val = UVal Text
         | NpcVal Text
         | ContextVal Text
         | GlobalVal Text
         | VarVal Text

instance ToJSON Val where
  toJSON v = case v of
               UVal t -> object [ "u_val" .= t ]
               NpcVal t -> object [ "npc_val" .= t ]
               ContextVal t -> object [ "context_val" .= t ]
               GlobalVal t -> object [ "global_val" .= t ]
               VarVal t -> object [ "var_val" .= t ]

data Math = Math1 MathExpr
             | Math2 MathExpr MathOp
             | Math3 MathExpr MathOp MathExpr

instance ToJSON Math where
  toJSON (Math1 (MathExpr e)) = object [ "math" .= [e] ]
  toJSON (Math2 (MathExpr e) (MathOp op)) = object [ "math" .= [e, op]]
  toJSON (Math3 (MathExpr e1) (MathOp op) (MathExpr e2)) = object [ "math" .= [e1, op, e2]]

newtype MathExpr = MathExpr Text
  deriving Generic

instance ToJSON MathExpr where

newtype MathOp = MathOp Text
  deriving Generic

instance ToJSON MathOp where

{-# DEPRECATED Op "replace Math" #-}
newtype Op = Op Text
  deriving Generic

instance ToJSON Op

{-# DEPRECATED Arithmetic "replace Math" #-}
data Arithmetic = ArithmeticAssign Val Arithmetic
                | ArithmeticOperation Arithmetic Op Arithmetic
                | ArithmeticConst Int
                | ArithmeticRand Int
                | ArithmeticVal Val

instance ToJSON Arithmetic where
  toJSON a =
    let f x = object [ "arithmetic" .= x ]
     in case a of
               ArithmeticAssign (UVal v) a' -> f
                [ object [ "u_val" .= ("var" :: Text)
                         , "var_name" .= v
                         ]
                , "="
                , toJSON a'
                ]
               ArithmeticAssign (NpcVal v) a' -> f
                [ object [ "npc_val" .= ("var" :: Text)
                         , "var_name" .= v
                         ]
                , "="
                , toJSON a'
                ]
               ArithmeticAssign (ContextVal _) _ -> undefined
               ArithmeticAssign (GlobalVal _) _ -> undefined
               ArithmeticAssign (VarVal _) _ -> undefined
               ArithmeticOperation a1 op a2 -> f
                [ toJSON a1
                , toJSON op
                , toJSON a2
                ]
               ArithmeticConst n ->
                 object [ "const" .= n ]
               ArithmeticRand n ->
                 object [ "rand" .= n ]
               ArithmeticVal (UVal v) ->
                 object [ "u_val" .= ("var" :: Text)
                        , "var_name" .= v
                        ]
               ArithmeticVal (NpcVal v) ->
                 object [ "npc_val" .= ("var" :: Text)
                        , "var_name" .= v
                        ]
               ArithmeticVal (ContextVal _) -> undefined
               ArithmeticVal (GlobalVal _) -> undefined
               ArithmeticVal (VarVal _) -> undefined

{-# DEPRECATED Var "replace Math" #-}
data Var = Var Text Text Text

{-# DEPRECATED CompareVar "replace Math" #-}
data CompareVar = UCompareVar Var Op Int
                | NpcCompareVar Var Op Int
                | UCompareTime Var Op Text
                | NpcCompareTime Var Op Text

data Condition = ConditionAnd [Condition]
               | ConditionOr [Condition]
               | ConditionNot Condition
               | ConditionMath Math
               | ConditionCompareVar CompareVar
               | ConditionNone
               | ConditionEffect Effect

instance Default Condition where
  def = ConditionNone

instance ToJSON Condition where
  toJSON c = case toJSONCondition c of
               Just c' -> c'
               Nothing -> toJSON (Nothing :: Maybe Text)

toJSONCondition :: Condition -> Maybe Value
toJSONCondition (ConditionAnd cs)          = Just $ object [ "and" .= map toJSONCondition cs ]
toJSONCondition (ConditionOr cs)           = Just $ object [ "or" .= map toJSONCondition cs ]
toJSONCondition (ConditionNot c)           = Just $ object [ "not" .= toJSONCondition c ]
toJSONCondition (ConditionMath m) = Just $ toJSON m
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
toJSONCondition (ConditionEffect e) = Just $ toJSON e

data EValue = EInt Int
            | EString Text
            | EBool Bool
            | EVal Val
            | EMath Math
            | ECondition Condition
            | EEffect Effect
            | EList [EValue]

instance ToJSON EValue where
  toJSON v = case v of
                EInt i -> toJSON i
                EString t -> toJSON t
                EBool b -> toJSON b
                EVal val -> toJSON val
                EMath m -> toJSON m
                ECondition c -> toJSON c
                EEffect e -> toJSON e
                EList es -> toJSON es

{-# DEPRECATED EffectArithmetic "replace Math" #-}
{-# DEPRECATED UAdjustVar "replace Math" #-}
{-# DEPRECATED NpcAdjustVar "replace Math" #-}
data Effect = EffectArithmetic Arithmetic
            | EffectMath Math
            | NpcCastSpell EValue EValue
            | UConsumeItem EValue EValue
            | UMessage Text Text Bool
            | UAdjustVar Var Int
            | NpcAdjustVar Var Int
            | UAddMorale Id Int Int Text Text
            | UAddEffect Id Int
            | NpcAddEffect Id Int
            | SetStringVar EValue EValue EValue
            | UHasItems EValue EValue
            | NpcHasFlag EValue
            | RunEocUntil EValue Id
            | RunEocs EValue
            | If EValue EValue (Maybe EValue)
            | SetCondition Id Condition
            | ULoseVar Val
            | NpcAddVar Val Text
            | NpcHasVar Val Text
            | Foreach Text Id Val [Effect]

instance ToJSON Effect where
  toJSON (EffectArithmetic arith  ) = toJSON arith
  toJSON (EffectMath m) = toJSON m
  toJSON (NpcCastSpell spellId b  ) = object [ "npc_cast_spell" .=
                                                  object [ "id" .= spellId
                                                         , "hit_self" .= b
                                                         ]
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
  toJSON (SetStringVar v t b) = object [ "set_string_var" .= t
                                       , "target_var" .= toJSON v
                                       , "parse_tags" .= b
                                       ]
  toJSON (UHasItems itemId n) = object
                            [ "u_has_items" .= object
                              [ "item" .= itemId
                              , "count" .= n
                              ]
                            ]

  toJSON (NpcHasFlag f) = object [ "npc_has_flag" .= f ]
  toJSON (RunEocUntil eoc c) = object [ "run_eoc_until" .= eoc
                                      , "condition" .= c
                                      , "iteration_count" .= (100 :: Int)
                                      ]
  toJSON (RunEocs eoc) = object [ "run_eocs" .= eoc ]
  toJSON (If c t (Just e)) = object [ "if" .= c
                                    , "then" .= t
                                    , "else" .= e
                                    ]
  toJSON (If c t Nothing) = object [ "if" .= c
                                   , "then" .= t
                                   ]
  toJSON (SetCondition condId cond) = object [ "set_condition" .= condId
                                             , "condition" .= cond
                                             ]
  toJSON (ULoseVar (UVal v)) = object [ "u_lose_var" .= v ]
  toJSON (ULoseVar _) = object [ "u_lose_var" .= ("undefined" :: T.Text) ]
  toJSON (NpcAddVar (NpcVal v) va) = object [ "npc_add_var" .= v
                                            , "value" .= va
                                            ]
  toJSON (NpcAddVar _ _) = object [ "npc_add_var" .= ("npc_add_var_undefined" :: T.Text)
                                  , "value" .= ("undef" :: T.Text)
                                  ]
  toJSON (NpcHasVar (NpcVal v) va) = object [ "npc_has_var" .= v
                                            , "value" .= va
                                            ]
  toJSON (NpcHasVar _ _) = object [ "npc_has_var" .= ("npc_has_var_undefined" :: T.Text)
                                  , "value" .= ("undef" :: T.Text)
                                  ]
  toJSON (Foreach t mgId v es) = object [ "foreach" .= t
                                   , "var" .= v
                                   , "effect" .= es
                                   , "target" .= mgId
                                   ]

class Expr e where
  toExpr :: e -> MathExpr

instance Expr Int where
  toExpr = MathExpr . T.pack . show

instance Expr Bool where
  toExpr True = MathExpr "1"
  toExpr False = MathExpr "0"

instance Expr Val where
  toExpr e = MathExpr $ case e of
              (UVal v')       -> "u_" <> v'
              (NpcVal v')     -> "n_" <> v'
              (ContextVal v') -> "_" <> v'
              (GlobalVal v')  -> v'
              (VarVal v')     -> "v_" <> v'

instance Expr MathExpr where
  toExpr = id

class ToEValue v where
  toEValue :: v -> EValue

instance ToEValue Int where
  toEValue = EInt

instance ToEValue Text where
  toEValue = EString

instance ToEValue Bool where
  toEValue = EBool

instance ToEValue Val where
  toEValue = EVal

instance ToEValue Id where
  toEValue (Id i) = toEValue i

instance ToEValue Condition where
  toEValue = ECondition

instance ToEValue Math where
  toEValue = EMath

instance ToEValue Effect where
  toEValue = EEffect

instance (ToEValue a) => ToEValue [a] where
  toEValue vs = EList $ map toEValue vs
