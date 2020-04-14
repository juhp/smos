{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.DirTree.Gen where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()
import qualified Data.Map as M
import Path
import Smos.Sync.Client.DirTree
import qualified System.FilePath as FP
import Test.QuickCheck

instance (Ord a, GenUnchecked a) => GenUnchecked (DirTree a)

instance (Ord a, GenUnchecked a, GenInvalid a) => GenInvalid (DirTree a)

instance (Ord a, GenValid a) => GenValid (DirTree a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (Ord a, GenUnchecked a) => GenUnchecked (DirForest a)

instance (Ord a, GenUnchecked a, GenInvalid a) => GenInvalid (DirForest a)

instance (Ord a, GenValid a) => GenValid (DirForest a) where
  genValid = DirForest . M.fromList <$> genListOf genPair
    where
      genPair =
        let genStr = genListOf (genValid `suchThat` (not . isUtf16SurrogateCodePoint))
         in oneof
              [ do rf <- filename <$> (genValid :: Gen (Path Rel File))
                  -- ((genStr `suchThatMap` parseRelFile) `suchThat` isValid :: Gen (Path Rel File))
                   dt <- NodeFile <$> genValid
                   pure (fromRelFile rf, dt)
              , do rd <- dirname <$> (genValid :: Gen (Path Rel Dir))
                  -- ((genStr `suchThatMap` (parseRelDir . (++ "/"))) `suchThat` isValid :: Gen (Path Rel Dir))
                   dt <- NodeDir <$> (genValid `suchThat` (not . M.null . unDirForest))
                   pure (FP.dropTrailingPathSeparator $ fromRelDir rd, dt)
              ]
  shrinkValid = shrinkValidStructurally
