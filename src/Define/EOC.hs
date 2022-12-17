module Define.EOC where

import Define.Core
import Define.Talk

data EOC = EOC
  { _eocId :: Id
  , _eocRecurrence :: Int
  , _eocCondition :: Condition
  , _eocDeactiveConditon :: Condition
  , _eocEffect :: Effect
  , _eocFalseEffect :: Effect
  }
