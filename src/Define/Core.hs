{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Define.Core
  ( Id(..)
  , Name
  , Description
  , FoodCategory
  , GradeType(..)
  , Grade(..)
  , ExpType(..)
  , Exp(..)
  , UseAction
  ) where

import Data.Default
import Data.Text (Text)

import Data.Aeson

instance Default Bool where
  def = False

instance Default Text where
  def = ""

newtype Id = Id
  { runId :: Text }
  deriving (Eq, Ord, Default)

instance ToJSON Id where
  toJSON = toJSON . runId

type Name = Text

type Description = Text

type FoodCategory = Text

data GradeType = GradeMeat1 | GradeMeat2 | GradeMarrow1 | GradeMarrow2
data Grade = Grade GradeType Int

data ExpType = ExpEarly | ExpNormal | ExpLate
newtype Exp = Exp ExpType

type UseAction = Text
