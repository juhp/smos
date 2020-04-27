{-# LANGUAGE DeriveGeneric #-}

-- |
--
-- Import this module qualified
module Smos.Sync.Client.MetaMap
  ( MetaMap (..),
    empty,
    singleton,
    fromList,
    insert,
    union,
    unions,
  )
where

import Control.DeepSeq
import Control.Monad
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Smos.Sync.Client.DirTree
import Smos.Sync.Client.Env

newtype MetaMap
  = MetaMap
      { metaMapFiles :: DirForest SyncFileMeta
      }
  deriving (Show, Eq, Generic)

instance Validity MetaMap where
  validate mm =
    mconcat
      [ genericValidate mm,
        declare "The uuids are distinct" $
          let distinct ls = nub ls == ls
           in distinct $ map syncFileMetaUUID $ M.elems $ dirForestToMap $ metaMapFiles mm
      ]

instance NFData MetaMap

empty :: MetaMap
empty = MetaMap emptyDirForest

singleton :: Path Rel File -> SyncFileMeta -> MetaMap
singleton k v = MetaMap $ singletonDirForest k v

fromList :: [(Path Rel File, SyncFileMeta)] -> Maybe MetaMap
fromList tups = do
  df <- case dirForestFromMap $ M.fromList tups of
    Left _ -> Nothing
    Right r -> Just r
  constructValid $ MetaMap df

insert :: Path Rel File -> SyncFileMeta -> MetaMap -> Maybe MetaMap
insert k v (MetaMap m) = do
  df <- case insertDirForest k v m of
    Left _ -> Nothing
    Right r -> Just r
  constructValid $ MetaMap df

union :: MetaMap -> MetaMap -> Maybe MetaMap
union (MetaMap m1) (MetaMap m2) =
  constructValid $ MetaMap $ unionDirForest m1 m2

unions :: [MetaMap] -> Maybe MetaMap
unions = constructValid . MetaMap . unionsDirForest . map metaMapFiles
