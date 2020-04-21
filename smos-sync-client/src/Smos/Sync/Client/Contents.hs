{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.Contents where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful
import qualified Data.Set as S
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import Path
import Path.IO
import Smos.API
import Smos.Report.Streaming
import Smos.Sync.Client.ContentsMap (ContentsMap (..))
import Smos.Sync.Client.DirTree
import Smos.Sync.Client.OptParse.Types

readFilteredSyncFiles :: IgnoreFiles -> Path Abs Dir -> IO ContentsMap
readFilteredSyncFiles igf dir = do
  let filterFunc =
        case igf of
          IgnoreNothing -> id
          IgnoreHiddenFiles -> filterHiddenDirForest
  -- TODO filtering during reading would be better!
  -- In fact it will be necessary
  filterFunc <$> readDirForest dir (SB.readFile . fromAbsFile)

readSyncFiles :: Path Abs Dir -> IO ContentsMap
readSyncFiles dir = readDirForest dir (SB.readFile . fromAbsFile)

filterContentsMap :: IgnoreFiles -> ContentsMap -> ContentsMap
filterContentsMap IgnoreNothing = id
filterContentsMap IgnoreHiddenFiles = filterDirForest (\p _ -> not $ isHidden p) -- TODO Correct this use of 'isHidden' correctly using the dirforest structure

makeContentsMap :: Mergeful.ClientStore FileUUID SyncFile -> Either (DirForestInsertionError ByteString) ContentsMap
makeContentsMap Mergeful.ClientStore {..} =
  dirForestFromMap
    $ M.fromList
    $ map (\SyncFile {..} -> (syncFilePath, syncFileContents))
    $ concat
      [ M.elems clientStoreAddedItems,
        M.elems $ M.map Mergeful.timedValue clientStoreSyncedItems,
        M.elems $ M.map Mergeful.timedValue clientStoreSyncedButChangedItems
      ]

saveContentsMap :: IgnoreFiles -> Path Abs Dir -> ContentsMap -> IO ()
saveContentsMap igf dir cm = do
  ensureDir dir
  let filePred =
        case igf of
          IgnoreNothing -> const True
          IgnoreHiddenFiles -> not . isHidden
      filterFunc =
        case igf of
          IgnoreNothing -> id
          IgnoreHiddenFiles -> M.filterWithKey (\p _ -> not $ isHidden p)
  let files = filterDirForest (\p _ -> filePred p) cm
  -- Remove the files that we find on disk that are not in the contents map
  found <- readDirForest dir (SB.readFile . fromAbsFile)
  forM_ (M.keys (dirForestToMap found)) $ \p -> case lookupDirForest p files of
    Nothing ->
      if filePred p
        then removeFile (dir </> p) -- Is not supposed to be there anymore
        else pure () -- We should leave it alone
    Just bs -> SB.writeFile (toFilePath $ dir </> p) bs -- TODO this seems a bit wasteful ..
  let leftovers = files `differenceDirForest` found
  ensureDir dir
  writeContentsMap dir (filterDirForest (\rf _ -> filePred rf) leftovers)

writeContentsMap :: Path Abs Dir -> ContentsMap -> IO ()
writeContentsMap dir df = do
  writeDirForest dir df (\p cts -> SB.writeFile (fromAbsFile p) cts)

isHidden :: Path Rel File -> Bool
isHidden = go
  where
    go :: Path Rel t -> Bool
    go f =
      if toFilePath f == "./"
        then False
        else
          let p = parent f
           in isHiddenIn p f || go p
