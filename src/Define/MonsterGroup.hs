{-# LANGUAGE OverloadedStrings #-}
module Define.MonsterGroup where

import Define.Core

import Data.Aeson

data MGMonster = MGMonster Id Int

instance ToJSON MGMonster where
  toJSON (MGMonster monId p) = object [ "monster" .= monId
                                      , "weight" .= p
                                      ]

data MonsterGroup = MonsterGroup Id [MGMonster]
