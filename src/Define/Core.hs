{-# LANGUAGE DeriveGeneric #-}
module Define.Core where

import GHC.Generics (Generic)
import Data.Text (Text)

import Define.Aeson

import Data.Aeson

newtype Id = Id Text
  deriving (Eq, Ord, Generic)

instance ToJSON Id

newtype Name = Name Text
  deriving Generic

instance ToJSON Name

newtype Description = Description Text
  deriving Generic

instance ToJSON Description

newtype FoodCategory = FoodCategory Text
  deriving Generic

instance ToJSON FoodCategory

data GradeType = GradeMeat1 | GradeMeat2 | GradeMarrow1 | GradeMarrow2
data Grade = Grade GradeType Int

data ExpType = ExpEarly | ExpNormal | ExpLate
newtype Exp = Exp ExpType

