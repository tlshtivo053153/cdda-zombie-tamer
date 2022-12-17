module Define.Core where

import Data.Text (Text)

newtype Id = Id Text
  deriving (Eq, Ord)
newtype Name = Name Text
newtype Description = Description Text

newtype FoodCategory = FoodCategory Text

data GradeType = GradeMeat1 | GradeMeat2 | GradeMarrow1 | GradeMarrow2
data Grade = Grade GradeType Int

data ExpType = ExpEarly | ExpNormal | ExpLate
newtype Exp = Exp ExpType

