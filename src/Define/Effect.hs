module Define.Effect
  ( Effect(..)
  ) where

import Data.Text

import Define.Core

data Effect = Effect
  { _effectId :: Id
  , _effectName :: [Text]
  , _effectDesc :: [Text]
  , _effectReducedDesc :: [Text]
  , _effectRating :: Text
  , _effectApplyMessage :: Text
  , _effectRemoveMessage :: Text
  }
