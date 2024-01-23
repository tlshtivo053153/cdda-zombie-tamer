{-# LANGUAGE OverloadedStrings #-}
module Cdda.Item where

import Define.Core
import Define.Item

import Cdda.Id.Item

import Data.Text (Text)
import qualified Data.Map as M

makeItem :: Id -> Id -> Name -> Description -> [UseAction] -> [FoodCategory] -> Item
makeItem = Item

useActionPetfood :: UseAction
useActionPetfood = UseAction "PETFOOD"

foodCategoryTMeat1 :: FoodCategory
foodCategoryTMeat2 :: FoodCategory
foodCategoryTMarrow1 :: FoodCategory
foodCategoryTMarrow2 :: FoodCategory

foodCategoryTMeat1 = FoodCategory "TAINTED_MEAT_FOOD1"
foodCategoryTMeat2 = FoodCategory "TAINTED_MEAT_FOOD2"
foodCategoryTMarrow1 = FoodCategory "TAINTED_MARROW_FOOD1"
foodCategoryTMarrow2 = FoodCategory "TAINTED_MARROW_FOOD2"

taintedMeatPremium :: Item
taintedMarrowPremium :: Item
taintedMeatHighPremium :: Item
taintedMarrowHighPremium :: Item
allPetfood :: [Item]

taintedMeatPremium = makeItem
  idTaintedMeat
  idTaintedMeatPremium
  (Name "上級汚染肉")
  (Description "上級汚染肉の説明")
  [ useActionPetfood ]
  [ foodCategoryTMeat1 ]
  
taintedMarrowPremium = makeItem
  idTaintedMarrow
  idTaintedMarrowPremium
  (Name "上級汚染骨髄")
  (Description "上級汚染骨髄の説明")
  [ useActionPetfood ]
  [ foodCategoryTMarrow1 ]

taintedMeatHighPremium = makeItem
  idTaintedMeat
  idTaintedMeatHighPremium
  (Name "最上級汚染肉")
  (Description "最上級汚染肉の説明")
  [ useActionPetfood ]
  [ foodCategoryTMeat2 ]

taintedMarrowHighPremium = makeItem
  idTaintedMarrow
  idTaintedMarrowHighPremium
  (Name "最上級汚染骨髄")
  (Description "最上級汚染骨髄の説明")
  [ useActionPetfood ]
  [ foodCategoryTMarrow2 ]

allPetfood =
  [ taintedMeatPremium
  , taintedMarrowPremium
  , taintedMeatHighPremium
  , taintedMarrowHighPremium
  ]

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
