{-# LANGUAGE OverloadedStrings #-}
module Cdda.Flag.Level
  ( levels
  , getLevel
  ) where

import Define.Flag

import qualified Data.Text as T

levels :: [Flag]
levels = map getLevel [1..100]

getLevel :: Int -> Flag
getLevel l = Flag $ "ZT_LEVEL_" <> T.pack (show l)
