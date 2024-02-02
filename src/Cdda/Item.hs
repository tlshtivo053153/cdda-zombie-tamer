{-# LANGUAGE OverloadedStrings #-}
module Cdda.Item
  ( idToName
  ) where

import Define.Core

import Cdda.Id.Item

import Data.Text (Text)
import qualified Data.Map as M

idToName :: Id -> Maybe Text
idToName itemId = M.lookup itemId $ M.fromList
  [ (,) idTaintedMeatPremium "上級汚染肉"
  , (,) idTaintedMeatHighPremium "最上級汚染肉"
  , (,) idTaintedMarrowPremium "上級汚染骨髄"
  , (,) idTaintedMarrowHighPremium "最上級汚染骨髄"
  , (,) idGasoline "ガソリン"
  , (,) idSheetKevlar "生地(ケブラー)"
  , (,) idMealBone "骨粉"
  , (,) idChemSulphuricAcid "硫酸"
  , (,) idScrap "金属片"
  , (,) idBurntOutBionic "壊れた整体部品"
  , (,) idVeggy "植物髄"
  , (,) idSlimeScrap "粘液の塊"
  ]
