module Define.Flag
  ( Flag(..)
  ) where

import Data.Text

newtype Flag = Flag Text
  deriving (Eq, Ord)
