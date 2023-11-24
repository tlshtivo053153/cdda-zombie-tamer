{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.Item where

import Define.Core

idTaintedMeat :: Id
idTaintedMarrow :: Id
idTaintedFat :: Id
idTaintedBone :: Id
idTaintedBlood :: Id
idSinew :: Id

idTaintedMeat = Id "meat_tainted"
idTaintedMarrow = Id "tainted_marrow"
idTaintedFat = Id "fat_tainted"
idTaintedBone = Id "bone_tainted"
idTaintedBlood = Id "blood_tainted"
idSinew = Id "sinew"

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
