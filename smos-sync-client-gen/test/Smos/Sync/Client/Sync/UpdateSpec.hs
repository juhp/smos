{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Sync.Client.Sync.UpdateSpec where

import qualified Data.ByteString as SB
import Data.Hashable
import qualified Data.Map as M
import Data.Mergeful.Timed as Mergeful
import Data.Pool
import qualified Data.Set as S
import Database.Persist.Sqlite as DB
import Pantry.SHA256 as SHA256
import Path
import Path.IO

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity

import Smos.Client
import Smos.Sync.Client.ContentsMap.Gen
import Smos.Sync.Client.DB
import Smos.Sync.Client.OptParse.Types
import Smos.Sync.Client.TestUtils
import Smos.Sync.Client.Update

spec :: Spec
spec =
  clientDBAndTestDirSpec $ do
    describe "updateUpdates" $ do
      describe "IgnoreNothing" $
        it "updates a file in the given set, even if it's hidden." $ \(pool, td) ->
          forAll genHiddenFile $ \p ->
            forAllValid $ \cts1 ->
              forAllValid $ \cts2 ->
                forAllValid $ \st1 ->
                  forAllValid $ \st2 ->
                    forAllValid $ \u -> do
                      let m =
                            M.singleton
                              u
                              (Mergeful.Timed
                                 SyncFile {syncFilePath = p, syncFileContents = cts2}
                                 st2)
                      testDB pool $
                        insert_
                          ClientFile
                            { clientFileUuid = u
                            , clientFilePath = p
                            , clientFileHash = Just $ hash cts1
                            , clientFileSha256 = Just $ SHA256.hashBytes cts1
                            , clientFileTime = st1
                            }
                      let fp = td </> p
                      ensureDir $ parent fp
                      SB.writeFile (fromAbsFile fp) cts1
                      testDB pool $ updateUpdates IgnoreNothing td m
                      mcf <- fmap (fmap entityVal) $ testDB pool $ getBy (UniqueUUID u)
                      mcf `shouldBe`
                        Just
                          ClientFile
                            { clientFileUuid = u
                            , clientFilePath = p
                            , clientFileHash = Just $ hash cts2
                            , clientFileSha256 = Just $ SHA256.hashBytes cts2
                            , clientFileTime = st2
                            }
      describe "IgnoreHidden" $
        it "updates a file in the given set, except if it's hidden." $ \(pool, td) ->
          forAll genHiddenFile $ \p ->
            forAllValid $ \cts1 ->
              forAllValid $ \cts2 ->
                forAllValid $ \st1 ->
                  forAllValid $ \st2 ->
                    forAllValid $ \u -> do
                      let m =
                            M.singleton
                              u
                              (Mergeful.Timed
                                 SyncFile {syncFilePath = p, syncFileContents = cts2}
                                 st2)
                      testDB pool $
                        insert_
                          ClientFile
                            { clientFileUuid = u
                            , clientFilePath = p
                            , clientFileHash = Just $ hash cts1
                            , clientFileSha256 = Just $ SHA256.hashBytes cts1
                            , clientFileTime = st1
                            }
                      let fp = td </> p
                      ensureDir $ parent fp
                      SB.writeFile (fromAbsFile fp) cts1
                      testDB pool $ updateUpdates IgnoreHiddenFiles td m
                      mcf <- fmap (fmap entityVal) $ testDB pool $ getBy (UniqueUUID u)
                      mcf `shouldBe`
                        Just
                          ClientFile
                            { clientFileUuid = u
                            , clientFilePath = p
                            , clientFileHash = Just $ hash cts1
                            , clientFileSha256 = Just $ SHA256.hashBytes cts1
                            , clientFileTime = st1
                            }
    describe "updateDeletions" $ do
      describe "IgnoreNothing" $
        it "deletes a file in the given set, even if it's hidden." $ \(pool, td) ->
          forAll genHiddenFile $ \f ->
            forAllValid $ \cts ->
              forAllValid $ \st ->
                forAllValid $ \u -> do
                  let s = S.singleton u
                  testDB pool $
                    insert_
                      ClientFile
                        { clientFileUuid = u
                        , clientFilePath = f
                        , clientFileHash = Just $ hash cts
                        , clientFileSha256 = Just $ SHA256.hashBytes cts
                        , clientFileTime = st
                        }
                  let fp = td </> f
                  ensureDir $ parent fp
                  SB.writeFile (fromAbsFile fp) cts
                  testDB pool $ updateDeletions IgnoreNothing td s
                  mcf <- fmap (fmap entityVal) $ testDB pool $ getBy (UniqueUUID u)
                  mcf `shouldBe` Nothing
                  forgivingAbsence (SB.readFile (fromAbsFile fp)) `shouldReturn` Nothing
      describe "IgnoreHidden" $
        it "deletes a file in the given set, except if it's hidden." $ \(pool, td) ->
          forAll genHiddenFile $ \f ->
            forAllValid $ \cts ->
              forAllValid $ \st ->
                forAllValid $ \u -> do
                  let s = S.singleton u
                  let cf =
                        ClientFile
                          { clientFileUuid = u
                          , clientFilePath = f
                          , clientFileHash = Just $ hash cts
                          , clientFileSha256 = Just $ SHA256.hashBytes cts
                          , clientFileTime = st
                          }
                  testDB pool $ insert_ cf
                  let fp = td </> f
                  ensureDir $ parent fp
                  SB.writeFile (fromAbsFile fp) cts
                  testDB pool $ updateDeletions IgnoreHiddenFiles td s
                  mcf <- fmap (fmap entityVal) $ testDB pool $ getBy (UniqueUUID u)
                  mcf `shouldBe` Just cf
                  forgivingAbsence (SB.readFile (fromAbsFile fp)) `shouldReturn` Just cts
  -- describe "IgnoreNothing" $
  --   it "removes any files that are not in the map" $ \d ->
  --     forAllValid $ \m1 ->
  --       forAll (mapWithAdditions m1) $ \m -> do
  --         setupContents d m
  --         saveContentsMap IgnoreNothing d m1
  --         assertContents d m1
  -- describe "IgnoreHiddenFiles" $ do
  --   forAllValid $ \m2 ->
  --     forAllValid $ \contents ->
  --       forAll (mapWithNewHiddenPath m2 contents) $ \(hp, m1) ->
  --         let anyHidden = not $ M.null $ contentsMapFiles $ filterHiddenFiles m1
  --          in cover 10.0 anyHidden "has any hidden files" $ do
  --               let hiddenFilesBefore = contentsMapFiles $ filterHiddenFiles m1
  --               setupContents d m1
  --               saveContentsMap IgnoreHiddenFiles d m2
  --               m3 <- readContents d
  --               let hiddenFilesAfter = contentsMapFiles $ filterHiddenFiles m3
  --               hiddenFilesAfter `shouldBe` M.singleton hp contents
  --               hiddenFilesAfter `shouldBe` hiddenFilesBefore
  --   it "leaves any hidden files, even if they are not in the map" $ \d ->
  --     checkCoverage $
  --     forAllValid $ \m2 ->
  --       forAll (mapWithHiddenAdditions m2) $ \m1 ->
  --         let anyHidden = not $ M.null $ contentsMapFiles $ filterHiddenFiles m1
  --          in cover 10.0 anyHidden "has any hidden files" $ do
  --               let hiddenFilesBefore = contentsMapFiles $ filterHiddenFiles m1
  --               setupContents d m1
  --               saveContentsMap IgnoreHiddenFiles d m2
  --               m3 <- readContents d
  --               let hiddenFilesAfter = contentsMapFiles $ filterHiddenFiles m3
  --               hiddenFilesAfter `shouldBe` hiddenFilesBefore

clientDBAndTestDirSpec :: SpecWith (Pool SqlBackend, Path Abs Dir) -> Spec
clientDBAndTestDirSpec = modifyMaxShrinks (const 0) . modifyMaxSuccess (`div` 10) . around go
  where
    go :: ((Pool SqlBackend, Path Abs Dir) -> IO a) -> IO a
    go func =
      withClientDB $ \pool ->
        withSystemTempDir "smos-sync-client-update-test" $ \td -> func (pool, td)

testDB :: Pool SqlBackend -> SqlPersistT IO a -> IO a
testDB = flip DB.runSqlPool
