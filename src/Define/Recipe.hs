module Define.Recipe where

import Define.Core

import Data.Text

data Quality = Quality Id Int

data Component = Component Id Int
               | ComponentList Id Int
type Components = [Component]

data Recipe = Recipe
  { _recipeResult :: Id
  , _recipeActivityLevel :: Text
  , _recipeCategory :: Text
  , _recipeSubcategory :: Text
  , _recipeDifficulty :: Int
  , _recipeTime :: Text
  , _recipeAutolearn :: Bool
  , _recipeQualities :: [Quality]
  , _recipeComponents :: [Components]
  }
