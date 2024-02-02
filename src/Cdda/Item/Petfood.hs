{-# LANGUAGE OverloadedStrings #-}
module Cdda.Item.Petfood
  ( foodCategoryTMeat1
  , foodCategoryTMeat2
  , foodCategoryTMarrow1
  , foodCategoryTMarrow2
  , taintedMeatPremium
  , taintedMarrowPremium
  , taintedMeatHighPremium
  , taintedMarrowHighPremium
  , allPetfood
  ) where

import Prelude hiding (id)
import Define.Core
import Define.Item.Petfood

import Cdda.Id.Item

import Define.MakeFields
import Control.Lens
import Data.Default

useActionPetfood :: UseAction
useActionPetfood = "PETFOOD"

foodCategoryTMeat1 :: FoodCategory
foodCategoryTMeat2 :: FoodCategory
foodCategoryTMarrow1 :: FoodCategory
foodCategoryTMarrow2 :: FoodCategory

foodCategoryTMeat1   = "TAINTED_MEAT_FOOD1"
foodCategoryTMeat2   = "TAINTED_MEAT_FOOD2"
foodCategoryTMarrow1 = "TAINTED_MARROW_FOOD1"
foodCategoryTMarrow2 = "TAINTED_MARROW_FOOD2"

taintedMeatPremium :: ItemPetfood
taintedMarrowPremium :: ItemPetfood
taintedMeatHighPremium :: ItemPetfood
taintedMarrowHighPremium :: ItemPetfood

taintedMeatPremium = def
  & copyFrom    .~ idTaintedMeat
  & id          .~ idTaintedMeatPremium
  & name        .~ "上級汚染肉"
  & description .~ "上級汚染肉の説明"
  & useAction   .~ [ useActionPetfood ]
  & petfood     .~ [ foodCategoryTMeat1 ]

taintedMarrowPremium = def
  & copyFrom    .~ idTaintedMarrow
  & id          .~ idTaintedMarrowPremium
  & name        .~ "上級汚染骨髄"
  & description .~ "上級汚染骨髄の説明"
  & useAction   .~ [ useActionPetfood ]
  & petfood     .~ [ foodCategoryTMarrow1 ]

taintedMeatHighPremium = def
  & copyFrom    .~ idTaintedMeat
  & id          .~ idTaintedMeatHighPremium
  & name        .~ "最上級汚染肉"
  & description .~ "最上級汚染肉の説明"
  & useAction   .~ [ useActionPetfood ]
  & petfood     .~ [ foodCategoryTMeat2 ]

taintedMarrowHighPremium = def
  & copyFrom    .~ idTaintedMarrow
  & id          .~ idTaintedMarrowHighPremium
  & name        .~ "最上級汚染骨髄"
  & description .~ "最上級汚染骨髄の説明"
  & useAction   .~ [ useActionPetfood ]
  & petfood     .~ [ foodCategoryTMarrow2 ]

allPetfood :: [ItemPetfood]
allPetfood =
  [ taintedMeatPremium
  , taintedMarrowPremium
  , taintedMeatHighPremium
  , taintedMarrowHighPremium
  ]
