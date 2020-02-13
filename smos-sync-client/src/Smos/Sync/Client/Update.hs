{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Sync.Client.Update where

import qualified Data.ByteString as SB
import Data.Hashable
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.UUID.Typed
import Data.Validity.UUID ()
import Path
import Path.IO

import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful

import Pantry.SHA256 as SHA256

import Control.Monad.Reader

import Database.Persist.Sql as DB

import Smos.Client

import Smos.Sync.Client.DB
import Smos.Sync.Client.OptParse.Types

updateClientAdded ::
     MonadIO m
  => Map Mergeful.ClientId SyncFile
  -> Map Mergeful.ClientId (Mergeful.ClientAddition FileUUID)
  -> SqlPersistT m ()
updateClientAdded r =
  fmap void . M.traverseWithKey $ \cid Mergeful.ClientAddition {..} ->
    case M.lookup cid r of
      Nothing -> pure () -- Should not happen, but not a big deal if it does.
      Just SyncFile {..} ->
        void $
        insertBy -- Insert into the database, assume that it worked
          ClientFile
            { clientFileUuid = clientAdditionId
            , clientFilePath = syncFilePath
            , clientFileHash = Just $ hash syncFileContents
            , clientFileSha256 = Just $ SHA256.hashBytes syncFileContents
            , clientFileTime = clientAdditionServerTime
            }
        -- No need to write to the file, the client added it so it's already there.

updateClientChanged :: MonadIO m => Map FileUUID Mergeful.ServerTime -> SqlPersistT m ()
updateClientChanged =
  fmap void . M.traverseWithKey $ \u st -> updateWhere [ClientFileUuid ==. u] [ClientFileTime =. st]
  -- No need to write to the file, the client changed it so it's already there.

updateClientDeleted :: MonadIO m => Set FileUUID -> SqlPersistT m ()
updateClientDeleted s = forM_ (S.toList s) $ \u -> deleteBy $ UniqueUUID u
  -- No need to delete the file because it was already deleted on the client side.

updateServerAdded ::
     MonadIO m
  => IgnoreFiles
  -> Path Abs Dir
  -> Map FileUUID (Mergeful.Timed SyncFile)
  -> SqlPersistT m ()
updateServerAdded = updateUpdates
  -- We have to use 'updateUpdates' here because the server doesn't know about the
  -- file path uniqueness constraint.

updateServerChanged ::
     MonadIO m
  => IgnoreFiles
  -> Path Abs Dir
  -> Map FileUUID (Mergeful.Timed SyncFile)
  -> SqlPersistT m ()
updateServerChanged = updateUpdates

updateServerDeleted :: MonadIO m => IgnoreFiles -> Path Abs Dir -> Set FileUUID -> SqlPersistT m ()
updateServerDeleted = updateDeletions

updateChangeConflict ::
     MonadIO m
  => IgnoreFiles
  -> Path Abs Dir
  -> Map FileUUID (Mergeful.Timed SyncFile)
  -> SqlPersistT m ()
updateChangeConflict = updateUpdates
  -- Take the server conflicts as updates
  -- We just take what the server gives

updateClientDeletedConflict ::
     MonadIO m
  => IgnoreFiles
  -> Path Abs Dir
  -> Map FileUUID (Mergeful.Timed SyncFile)
  -> SqlPersistT m ()
updateClientDeletedConflict = updateUpdates
  -- Take the server conflicts as updates
  -- We just take what the server gives and ignore that it was deleted client-side.

updateServerDeletedConflict ::
     MonadIO m => IgnoreFiles -> Path Abs Dir -> Set FileUUID -> SqlPersistT m ()
updateServerDeletedConflict = updateDeletions
  -- Take the server deletions as deletions
  -- We just take what the server gives and ignore that it was updated client-side.

updateUpdates ::
     MonadIO m
  => IgnoreFiles
  -> Path Abs Dir
  -> Map FileUUID (Mergeful.Timed SyncFile)
  -> SqlPersistT m ()
updateUpdates _ d =
  fmap void . M.traverseWithKey $ \u (Mergeful.Timed SyncFile {..} st) -> do
    let h = Just $ hash syncFileContents
        sha = Just $ SHA256.hashBytes syncFileContents
    void $
      upsertBy
        (UniquePath syncFilePath) -- Insert into the database, assume that it worked
        (ClientFile
           { clientFileUuid = u
           , clientFilePath = syncFilePath
           , clientFileHash = h
           , clientFileSha256 = sha
           , clientFileTime = st
           })
        [ClientFileUuid =. u, ClientFileHash =. h, ClientFileSha256 =. sha, ClientFileTime =. st]
    let fp = d </> syncFilePath
    liftIO $ SB.writeFile (fromAbsFile fp) syncFileContents

updateDeletions :: MonadIO m => IgnoreFiles -> Path Abs Dir -> Set FileUUID -> SqlPersistT m ()
updateDeletions _ d s =
  forM_ (S.toList s) $ \u -> do
    mcf <- getBy $ UniqueUUID u
    case mcf of
      Nothing ->
        error $
        "Did not find the metadata for the item that the server told us to delete: " <> uuidString u
      Just (Entity i ClientFile {..}) -> do
        let fp = d </> clientFilePath
        delete i
        liftIO $ removeFile fp
