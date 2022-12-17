{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.Friend
  ( monFriend ) where

import Define.Core

import qualified Data.Text as T

monFriend :: Id -> Int -> Id
monFriend (Id monId) lv = Id $ monId <> "_friend_lv" <> T.pack (show lv)
