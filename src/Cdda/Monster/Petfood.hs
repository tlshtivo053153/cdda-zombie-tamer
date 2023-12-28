module Cdda.Monster.Petfood
  ( getPetFood
  , getFriendCost
  , costToPetfood
  ) where

import Prelude hiding ( exp )
import Define.Core
import Define.Monster
import Define.MakeFields

import Cdda.Item.Petfood

import Cdda.Monster.Strength
import Cdda.Monster.Growth

import Control.Lens

foodAll :: [FoodCategory]
foodAll =
  [ foodCategoryTMeat1
  , foodCategoryTMarrow1
  , foodCategoryTMeat2
  , foodCategoryTMarrow2
  ]

getPetFood :: Id -> PetFood
getPetFood monId = case getFriendCost monId of
                     Nothing -> PetFood []
                     Just x -> costToPetfood x

getFriendCost :: Id -> Maybe Int
getFriendCost monId = do
  s <- do
    (Strength _s) <- getStrength monId
    return $ _s - 1
  (Growth x y) <- view exp <$> getGrowth monId
  return $ floor $ 10 * x^s + y*fromIntegral s

costToPetfood :: Int -> PetFood
costToPetfood cost | cost <= 15    = PetFood foodAll
                   | cost <= 120   = PetFood $ drop 1 foodAll
                   | cost <= 3000  = PetFood $ drop 2 foodAll
                   | cost <= 24000 = PetFood $ drop 3 foodAll
                   | otherwise     = PetFood []
