{-# LANGUAGE OverloadedStrings #-}
module Cdda.Spell.TerTransform
  ( spellPlaceMeatSlime
  , spellPlaceMarrowSlime
  ) where

import Prelude hiding (id)
import Define.Spell
import Define.Monster
import Define.MakeFields

import Cdda.Id.Spell
import Cdda.Id.TerFurnTransform

import qualified Data.Text as T
import Data.Default
import Control.Lens

spellPlaceMeatSlime :: Strength -> SpellTerTransform
spellPlaceMeatSlime s = def
  & id          .~ idSpellPlaceMeatSlime s
  & name        .~ "肉スライムを配置"
  & description .~ ("肉スライム" <> T.pack (show s) <> "を配置")
  & effectStr   .~ idTransPlaceMeatSlime s

spellPlaceMarrowSlime :: Strength -> SpellTerTransform
spellPlaceMarrowSlime s = def
  & id          .~ idSpellPlaceMarrowSlime s
  & name        .~ "骨髄スライムを配置"
  & description .~ ("骨髄スライム" <> T.pack (show s) <> "を配置")
  & effectStr   .~ idTransPlaceMarrowSlime s
