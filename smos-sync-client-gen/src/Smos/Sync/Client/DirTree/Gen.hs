{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.DirTree.Gen where

import Data.GenValidity
import Smos.API.Gen ()
import Smos.Sync.Client.DirTree

instance (Ord a, GenValid a) => GenValid (DirForest a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (Ord a, GenValid a) => GenValid (DirTree a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
