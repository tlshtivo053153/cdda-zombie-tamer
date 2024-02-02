{-# LANGUAGE DeriveGeneric, DeriveAnyClass  #-}
module Define.DeathFunction
  ( DeathFunction(..)
  ) where

import Define.Core

import GHC.Generics (Generic)
import Data.Default
import qualified Data.Text as T

data DeathFunction = DeathFunction
  { _deathFunctionId :: Maybe Id
  , _deathFunctionHitSelf :: Maybe Bool
  , _deathFunctionMinLevel :: Maybe Int
  , _deathFunctionCorpseType :: Maybe T.Text
  , _deathFunctionMessage :: Maybe T.Text
  }
  deriving (Generic, Default)
