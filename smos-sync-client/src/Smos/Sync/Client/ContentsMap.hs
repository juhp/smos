{-# LANGUAGE DeriveGeneric #-}

-- |
--
-- Import this module qualified
module Smos.Sync.Client.ContentsMap
  ( ContentsMap (..),
    empty,
    singleton,
    insert,
    fromList,
    toList,
    union,
    unions,
    contentsMapFiles,
  )
where

import Control.DeepSeq
import Control.Monad
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Smos.Sync.Client.DirTree as DT

type ContentsMap = DirForest ByteString

empty :: ContentsMap
empty = emptyDirForest

singleton :: Path Rel File -> ByteString -> ContentsMap
singleton = singletonDirForest

insert :: Path Rel File -> ByteString -> ContentsMap -> Maybe ContentsMap
insert rp cs cm = case insertDirForest rp cs cm of
  Left _ -> Nothing
  Right r -> Just r

fromList :: [(Path Rel File, ByteString)] -> Maybe ContentsMap
fromList tups = case dirForestFromList tups of
  Left _ -> Nothing
  Right cm -> Just cm

toList :: ContentsMap -> [(Path Rel File, ByteString)]
toList = dirForestToList

union :: ContentsMap -> ContentsMap -> ContentsMap
union = unionDirForest

unions :: [ContentsMap] -> ContentsMap
unions = unionsDirForest

contentsMapFiles :: ContentsMap -> Map (Path Rel File) ByteString
contentsMapFiles = dirForestToMap
