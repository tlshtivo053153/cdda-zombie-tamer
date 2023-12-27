{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.ItemGroup
  ( idItemGroupZombie
  , idItemGroupSkeleton
  ) where

import Define.Core

import qualified Data.Text as T

idItemGroupZombie :: Int -> Id
idItemGroupZombie n = Id $ "item_group_zombie_" <> T.pack (show n)

idItemGroupSkeleton :: Int -> Id
idItemGroupSkeleton n = Id $  "item_group_skeleton_" <> T.pack (show n)
