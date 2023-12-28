{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.Furniture
  ( idFurnitureNull
  , idMeatSlime
  , idMarrowSlime
  ) where

import qualified Data.Text as T

import Define.Core
import Define.Monster

idFurnitureNull :: Id
idFurnitureNull = Id "f_null"

idMeatSlime :: Strength -> Id
idMeatSlime n = Id $ "f_meat_slime_" <> T.pack (show n)

idMarrowSlime :: Strength -> Id
idMarrowSlime n = Id $ "f_marrow_slime_" <> T.pack (show n)
