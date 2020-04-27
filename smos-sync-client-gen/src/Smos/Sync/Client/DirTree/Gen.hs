{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.DirTree.Gen where

import Data.GenValidity
import Smos.API.Gen ()
import Smos.Sync.Client.DirTree
import Test.QuickCheck

instance (Ord a, GenValid a) => GenValid (DirForest a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (Ord a, GenValid a) => GenValid (DirTree a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

changedDirForest :: (Ord a, GenValid a) => DirForest a -> Gen (DirForest a)
changedDirForest = traverse (\v -> genValid `suchThat` (/= v))

disjunctMap :: (Ord a, GenValid a) => DirForest a -> Gen (DirForest a)
disjunctMap m = genValid `suchThat` (\m' -> nullDirForest $ intersectionDirForest m m')
