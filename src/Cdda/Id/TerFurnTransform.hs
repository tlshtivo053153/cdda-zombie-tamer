{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.TerFurnTransform
  ( idTransPlaceMeatSlime
  , idTransPlaceMarrowSlime
  ) where

import qualified Data.Text as T
import Define.Core
import Define.Monster

idTransPlaceMeatSlime :: Strength -> Id
idTransPlaceMeatSlime n = Id $ "trans_place_meat_slime_" <> T.pack (show n)

idTransPlaceMarrowSlime :: Strength -> Id
idTransPlaceMarrowSlime n = Id $ "trans_place_marrow_slime_" <> T.pack (show n)
