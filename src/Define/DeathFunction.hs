module Define.DeathFunction where

import Define.Core

import qualified Data.Text as T

data DeathFunction = DeathFunction
  { _deathFunctionId :: Maybe Id
  , _deathFunctionHitSelf :: Maybe Bool
  , _deathFunctionMinLevel :: Maybe Int
  , _deathFunctionCorpseType :: Maybe T.Text
  , _deathFunctionMessage :: Maybe T.Text
  }
