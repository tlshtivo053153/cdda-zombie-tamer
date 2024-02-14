{-# LANGUAGE OverloadedStrings #-}
module Cdda.Id.MonsterGroup
  ( idFriendBase
  , idNormal
  , idFat
  , idMedical
  , idBoomer
  , idRust
  , idLabsecurity
  , idElectric
  , idSkeleton
  , randomUpgradeToId
  ) where

import Define.Core
import Define.Monster

idFriendBase :: Id
idFriendBase = Id "GROUP_FRIEND_ZOMBIE_BASE"

idNormal :: Id
idFat :: Id
idMedical :: Id
idBoomer :: Id
idRust :: Id
idLabsecurity :: Id
idElectric :: Id
idSkeleton :: Id

idNormal      = Id "GROUP_FRIEND_ZOMBIE_UPGRADE"
idFat         = Id "GROUP_FRIEND_ZOMBIE_FAT_UPGRADE"
idMedical     = Id "GROUP_FRIEND_ZOMBIE_MEDICALUPGRADE"
idBoomer      = Id "GROUP_FRIEND_ZOMBIE_BOOMER_UPGRADE"
idRust        = Id "GROUP_FRIEND_ZOMBIE_RUST_UPGRADE"
idLabsecurity = Id "GROUP_FRIEND_ZOMBIE_LABSECURITY_UPGRADE"
idElectric    = Id "GROUP_FRIEND_ZOMBIE_ELECTRIC_UPGRADE"
idSkeleton    = Id "GROUP_FRIEND_SKELETON_UPGRADE"

randomUpgradeToId :: UpgradeRandomType -> Maybe Id
randomUpgradeToId urt = case urt of
                          URNormal      -> Just idNormal
                          URFat         -> Just idFat
                          URMedical     -> Just idMedical
                          URBoomer      -> Just idBoomer
                          URRust        -> Just idRust
                          URLabsecurity -> Just idLabsecurity
                          URElectric    -> Just idElectric
                          URSkeleton    -> Just idSkeleton
                          URNone        -> Nothing

