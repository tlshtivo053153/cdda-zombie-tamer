{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.Item where

import Define.Core

idTaintedMeat :: Id
idTaintedMarrow :: Id

idTaintedMeat = Id "tainted_meat"
idTaintedMarrow = Id "tainted_marrow"

idTaintedMeatPremium :: Id
idTaintedMarrowPremium :: Id
idTaintedMeatHighPremium :: Id
idTaintedMarrowHighPremium :: Id

idTaintedMeatPremium = Id "tainted_meat_premium"
idTaintedMarrowPremium = Id "tainted_marrow_premium"
idTaintedMeatHighPremium = Id "tainted_meat_high_premium"
idTaintedMarrowHighPremium = Id "tainted_marrow_high_premium"

idGasoline :: Id
idSheetKevlar :: Id
idMealBone :: Id
idChemSulphuricAcid :: Id
idScrap :: Id
idBurntOutBionic :: Id
idVeggy :: Id
idSlimeScrap :: Id

idGasoline = Id "gasoline"
idSheetKevlar = Id "sheet_kevlar"
idMealBone = Id "meal_bone"
idChemSulphuricAcid = Id "chem_sulphuric_acid"
idScrap = Id "scrap"
idBurntOutBionic = Id "burnt_out_bionic"
idVeggy = Id "veggy"
idSlimeScrap = Id "slime_scrap"
