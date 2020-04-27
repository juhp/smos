{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Sync.Client.DirTree
  ( DirTree (..),
    DirForest (..),
    emptyDirForest,
    singletonDirForest,
    lookupDirForest,
    insertDirForest,
    dirForestFromList,
    dirForestToList,
    unionDirForest,
    unionsDirForest,
    nullDirForest,
    intersectionDirForest,
    filterDirForest,
    filterHiddenDirForest,
    differenceDirForest,
    DirForestInsertionError (..),
    dirForestFromMap,
    dirForestToMap,
    readDirForest,
    readFilteredDirForest,
    writeDirForest,
  )
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Mergeful.Item
import Data.Mergeful.Timed
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Map
import Data.Validity.Path
import Data.Word
import Debug.Trace
import GHC.Generics (Generic)
import Path
import Path.IO
import qualified System.FilePath as FP

data DirTree a
  = NodeFile a
  | NodeDir (DirForest a)
  deriving (Show, Eq, Ord, Generic, Functor)

instance (Validity a, Ord a) => Validity (DirTree a)

instance (NFData a, Ord a) => NFData (DirTree a)

instance Foldable DirTree where
  foldMap func =
    \case
      NodeFile v -> func v
      NodeDir df -> foldMap func df

instance Traversable DirTree where
  traverse func =
    \case
      NodeFile v -> NodeFile <$> func v
      NodeDir df -> NodeDir <$> traverse func df

newtype DirForest a
  = DirForest
      { unDirForest :: Map FilePath (DirTree a)
      } -- TODO change 'FilePath' to something more sensible like a FileOrDir, maybe?
  deriving (Show, Eq, Ord, Generic, Functor)

instance (Validity a, Ord a) => Validity (DirForest a) where
  validate df@(DirForest m) =
    mconcat
      [ genericValidate df,
        decorateMap m $ \p dt ->
          let isTopLevel p = parent p == [reldir|./|]
           in case dt of
                NodeFile _ ->
                  case parseRelFile p of
                    Nothing -> invalid $ "cannot parse as a relative directory: " <> p
                    Just rf ->
                      mconcat
                        [ decorate "The can path can be parsed as a valid relative dir path" $
                            mconcat
                              [ declare "and to the same path, no less" $ fromRelFile rf == p,
                                validate rf
                              ],
                          declare "There are no separators on this level" $ isTopLevel rf
                        ]
                NodeDir (DirForest df') ->
                  mconcat
                    [ declare "The contained dirforest is nonempty" $ not $ M.null df',
                      declare "the path has no trailing path separator"
                        $ not
                        $ FP.hasTrailingPathSeparator p,
                      case parseRelDir p of
                        Nothing -> invalid $ "cannot parse as a relative directory: " <> p
                        Just rd ->
                          mconcat
                            [ decorate "The can path can be parsed as a valid relative dir path" $
                                mconcat
                                  [ declare "and to the same path, no less" $
                                      FP.dropTrailingPathSeparator (fromRelDir rd) == p,
                                    validate rd
                                  ],
                              declare "There are no separators on this level" $ isTopLevel rd
                            ]
                    ]
      ]

instance (NFData a, Ord a) => NFData (DirForest a)

instance Foldable DirForest where
  foldMap func (DirForest dtm) = foldMap (foldMap func) dtm

instance Traversable DirForest where
  traverse func (DirForest dtm) = DirForest <$> traverse (traverse func) dtm

emptyDirForest :: DirForest a
emptyDirForest = DirForest M.empty

singletonDirForest :: Ord a => Path Rel File -> a -> DirForest a
singletonDirForest rp a =
  case insertDirForest rp a emptyDirForest of
    Right df -> df
    _ -> error "There can't have been anything in the way in an empty dir forest."

lookupDirForest ::
  forall a.
  Ord a =>
  Path Rel File ->
  DirForest a ->
  Maybe a
lookupDirForest rp df = go df (FP.splitDirectories $ fromRelFile rp)
  where
    go :: DirForest a -> [FilePath] -> Maybe a
    go (DirForest ts) =
      \case
        [] -> Nothing
        [f] -> do
          dt <- M.lookup f ts
          case dt of
            NodeFile contents -> Just contents
            _ -> Nothing
        (d : ds) -> do
          dt <- M.lookup d ts
          case dt of
            NodeDir dt -> go dt ds
            _ -> Nothing

insertDirForest ::
  forall a.
  Ord a =>
  Path Rel File ->
  a ->
  DirForest a ->
  Either (DirForestInsertionError a) (DirForest a)
insertDirForest rp a df = go [reldir|./|] df (FP.splitDirectories $ fromRelFile rp)
  where
    go ::
      Path Rel Dir ->
      DirForest a ->
      [FilePath] ->
      Either (DirForestInsertionError a) (DirForest a)
    go cur (DirForest ts) =
      \case
        [] -> Right df -- Should not happen, but just insert nothing if it does.
        [f] ->
          case M.lookup f ts of
            Nothing -> pure $ DirForest $ M.insert f (NodeFile a) ts
            Just dt ->
              case dt of
                NodeFile contents -> do
                  let rf = cur </> fromJust (parseRelFile f)
                  Left (FileInTheWay rf contents)
                NodeDir df' -> do
                  let rd = cur </> fromJust (parseRelDir f)
                  Left (DirInTheWay rd df')
        (d : ds) ->
          case M.lookup d ts of
            Nothing -> do
              let rf = fromJust $ parseRelFile $ FP.joinPath ds -- Cannot fail if the original filepath is valid
              pure $ DirForest $ M.insert d (NodeDir $ singletonDirForest rf a) ts
            Just dt ->
              case dt of
                NodeFile contents -> do
                  let rf = cur </> fromJust (parseRelFile d)
                  Left (FileInTheWay rf contents)
                NodeDir df' -> do
                  let newCur = cur </> fromJust (parseRelDir d)
                  df'' <- go newCur df' ds
                  pure $ DirForest $ M.insert d (NodeDir df'') ts

dirForestFromList :: Ord a => [(Path Rel File, a)] -> Either (DirForestInsertionError a) (DirForest a)
dirForestFromList = foldM (flip $ uncurry insertDirForest) emptyDirForest

dirForestToList :: Ord a => DirForest a -> [(Path Rel File, a)]
dirForestToList = M.toList . dirForestToMap

unionDirForest :: Ord a => DirForest a -> DirForest a -> Either (DirForestInsertionError a) (DirForest a)
unionDirForest df1 df2 = foldM (flip $ uncurry insertDirForest) df1 $ dirForestToList df2

unionsDirForest :: Ord a => [DirForest a] -> Either (DirForestInsertionError a) (DirForest a)
unionsDirForest = foldM unionDirForest emptyDirForest

nullDirForest :: DirForest a -> Bool
nullDirForest (DirForest dtm) = M.null dtm

intersectionDirForest :: DirForest a -> DirForest b -> DirForest a
intersectionDirForest (DirForest dtm1) (DirForest dtm2) = DirForest $ M.intersectionWith intersectionDirTree dtm1 dtm2
  where
    intersectionDirTree :: DirTree a -> DirTree b -> DirTree a
    intersectionDirTree dt1 dt2 = case (dt1, dt2) of
      (NodeFile v, _) -> NodeFile v -- TODO is this what we want?
      (NodeDir df1, _) -> NodeDir df1 -- TODO is this what we want?
      (NodeDir df1, NodeDir df2) -> NodeDir $ intersectionDirForest df1 df2

filterDirForest :: forall a. Show a => (Path Rel File -> a -> Bool) -> DirForest a -> DirForest a
filterDirForest filePred = fromMaybe emptyDirForest . goForest "" -- Because "" FP.</> "anything" = "anything"
  where
    goForest :: FilePath -> DirForest a -> Maybe (DirForest a)
    goForest base (DirForest df) =
      let df' =
            M.mapMaybeWithKey
              (\p dt -> goTree (base FP.</> p) dt)
              df
       in if M.null df'
            then Nothing
            else Just (DirForest df')
    goTree :: FilePath -> DirTree a -> Maybe (DirTree a) -- Nothing means it will be removed
    goTree base dt = case dt of
      NodeFile cts -> do
        rf <- parseRelFile base
        if filePred rf cts then Just dt else Nothing
      NodeDir df -> NodeDir <$> goForest base df

filterHiddenDirForest :: forall a. DirForest a -> DirForest a
filterHiddenDirForest = fromMaybe emptyDirForest . goForest
  where
    goPair :: FilePath -> DirTree a -> Maybe (DirTree a)
    goPair fp dt = if hidden fp then Nothing else goTree dt
    goForest :: DirForest a -> Maybe (DirForest a)
    goForest (DirForest m) =
      let m' = M.mapMaybeWithKey goPair m
       in if M.null m' then Nothing else Just (DirForest m')
    goTree :: DirTree a -> Maybe (DirTree a)
    goTree dt = case dt of
      NodeFile _ -> Just dt
      NodeDir df -> NodeDir <$> goForest df
    hidden [] = False -- Technically not possible, but fine
    hidden ('.' : _) = True
    hidden _ = False

differenceDirForest :: forall a b. DirForest a -> DirForest b -> DirForest a
differenceDirForest = goForest "" -- Because "" </> "anything" = "anything"
  where
    goForest :: FilePath -> DirForest a -> DirForest b -> DirForest a
    goForest base (DirForest df1) (DirForest df2) =
      DirForest $ M.differenceWithKey (\p dt1 dt2 -> goTree (base FP.</> p) dt1 dt2) df1 df2
    goTree :: FilePath -> DirTree a -> DirTree b -> Maybe (DirTree a)
    goTree base dt1 dt2 = case (dt1, dt2) of
      (NodeFile _, _) -> Nothing -- TODO do we want to distinguisg between the two cases on the right?
      (NodeDir df, NodeFile _) -> Nothing -- TODO not sure that these are the right semantics
      (NodeDir df1, NodeDir df2) -> Just $ NodeDir $ goForest base df1 df2

data DirForestInsertionError a
  = FileInTheWay (Path Rel File) a
  | DirInTheWay (Path Rel Dir) (DirForest a)
  deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (DirForestInsertionError a)

-- TODO we'd like a list of errors, ideally
dirForestFromMap :: Ord a => Map (Path Rel File) a -> Either (DirForestInsertionError a) (DirForest a)
dirForestFromMap = foldM (\df (rf, cts) -> insertDirForest rf cts df) emptyDirForest . M.toList

dirForestToMap :: DirForest a -> Map (Path Rel File) a
dirForestToMap = M.foldlWithKey go M.empty . unDirForest
  where
    go :: Map (Path Rel File) a -> FilePath -> DirTree a -> Map (Path Rel File) a
    go m path =
      \case
        NodeFile contents ->
          let rf = fromJust (parseRelFile path) -- Cannot fail if the original dirforest is valid
           in M.insert rf contents m
        NodeDir df ->
          let rd = fromJust (parseRelDir path) -- Cannot fail if the original dirforest is valid
           in M.union m $ M.mapKeys (rd </>) (dirForestToMap df)

readDirForest ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readDirForest = readFilteredDirForest (const True)

readFilteredDirForest ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  (Path b File -> Bool) ->
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readFilteredDirForest filePred root readFunc = do
  mFiles <- liftIO $ forgivingAbsence $ snd <$> listDirRecurRel root
  foldM go emptyDirForest $ fromMaybe [] mFiles
  where
    go df p =
      let path = root </> p
       in if filePred path
            then do
              contents <- readFunc path
              case insertDirForest p contents df of
                Left e ->
                  error
                    "There can't have been anything in the way while reading a dirforest, but there was."
                Right df' -> pure df'
            else pure df

writeDirForest ::
  forall a b.
  Ord a =>
  Path b Dir ->
  DirForest a ->
  (Path b File -> a -> IO ()) ->
  IO ()
writeDirForest root dirForest writeFunc =
  forM_ (M.toList $ dirForestToMap dirForest) $ \(path, contents) -> do
    let f = root </> path
    ensureDir $ parent f
    writeFunc f contents
