{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Define.Talk
  ( DynamicLine(..)
  , Response(..)
  , Talk(..)
  , TalkConfig(..)
  , TalkState(..)
  , TalkAction
  ) where

import GHC.Generics (Generic)

import Data.Default
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Set as S
import Control.Monad.Reader

import Data.Aeson

import Define.Core
import Define.Monster
import Define.EOC

{-# DEPRECATED DynamicLineCompare "replace Math" #-}
data DynamicLine
    = DynamicLineCompare CompareVar DynamicLine DynamicLine
    | DynamicLineText Text
    | DynamicLineConcatenate [DynamicLine]

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
      DynamicLineConcatenate ts -> toJSON ts
      DynamicLineText t -> toJSON t

instance Default DynamicLine where
  def = DynamicLineText ""

data Response = Response
  { _responseCondition :: Condition
  , _responseText :: Text
  , _responseTrial :: Condition
  , _responseSuccess :: Talk
  , _responseSuccessEffect :: [Effect]
  , _responseFailure :: Maybe Talk
  , _responseFailureEffect :: [Effect]
  }
  deriving (Generic, Default)

data Talk = TalkBack
          | TalkDone
          | TalkTop
          | Talk
  { _talkTalkId :: Id
  , _talkSpeakerEffect :: [Effect]
  , _talkDynamicLine :: DynamicLine
  , _talkResponses :: [Response]
  }

instance Default Talk where
  def = Talk
    { _talkTalkId        = def
    , _talkSpeakerEffect = def
    , _talkDynamicLine   = def
    , _talkResponses     = def
    }

data TalkConfig = TalkConfig
  { _talkConfigMonsterId :: Id
  , _talkConfigMonsterBase :: Id
  , _talkConfigPetfood :: PetFood
  , _talkConfigStatus :: Status
  , _talkConfigLevel :: Int
  , _talkConfigNeedExp :: V.Vector Int
  , _talkConfigTopTalkId :: Id
  }

instance Default TalkConfig where
  def = TalkConfig
    { _talkConfigMonsterId       = def
    , _talkConfigMonsterBase     = def
    , _talkConfigPetfood         = PetFood []
    , _talkConfigStatus          = def
    , _talkConfigLevel           = 1
    , _talkConfigNeedExp         = V.empty
    , _talkConfigTopTalkId       = def
    }

data TalkState = TalkState
  { backId :: Id
  , stackId :: [Id]
  , setId :: S.Set Id
  }

instance Default TalkState where
  def = TalkState
    { backId  = Id "TALK_DONE"
    , stackId = def
    , setId   = def
    }

type TalkAction a = Reader TalkConfig  a
