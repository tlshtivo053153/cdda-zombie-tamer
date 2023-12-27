{-# LANGUAGE MultiWayIf #-}
module Cdda.Monster.Exp
  ( calcExp
  , itemExp ) where

import Prelude hiding (exp)
import Define.Core
import Define.Monster
import Define.MakeFields

import Cdda.Id.Item

import Control.Lens

initialExp :: Monster -> Int
initialExp mon =
  let (Strength str) = mon^.strength
   in (floor :: Rational -> Int) $ 10 * 1.12^(str-1)

calcExp :: Monster -> Int -> Int
calcExp mon l =
  let (Growth x y) = mon^.growth.exp
   in floor $ fromIntegral (initialExp mon) *x^(l-1) + y*fromIntegral (l-1)

itemExp :: Id -> Int
itemExp idItem =
  if
     | idItem == idTaintedMeatPremium -> 15
     | idItem == idTaintedMeatHighPremium -> 3000
     | idItem == idTaintedMarrowPremium -> 120
     | idItem == idTaintedMarrowHighPremium -> 24000
     | otherwise -> 0
