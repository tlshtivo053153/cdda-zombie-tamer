module Define.Flag
  ( Flag(..)
  ) where

import Data.Text

newtype Flag = Flag { runFlag :: Text }
  deriving (Eq, Ord)
