{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.ByteString (ByteString)
import Data.GenValidity.Criterion
import Smos.Sync.Client.ContentsMap
import Smos.Sync.Client.ContentsMap.Gen
import Smos.Sync.Client.DirTree
import Smos.Sync.Client.DirTree.Gen
import Smos.Sync.Client.Env
import Smos.Sync.Client.MetaMap
import Smos.Sync.Client.MetaMap.Gen ()
import Test.QuickCheck

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @SyncFileMeta
    , genValidBench @MetaMap
    , genValidBench @ContentsMap
    , genValidBench @ClientStore
    , genValidBench @(DirTree ByteString)
    , genValidBench @(DirForest ByteString)
    , genBench "sizedContentsMap 1000" (sizedContentsMap 1000)
    , genBench "sizedDirForest 1000" (sizedDirForest 1000 :: Gen (DirForest ByteString))
    ]
