{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.ContentsMap.Gen where

import Data.ByteString
import Data.GenValidity
import qualified Data.Map as M
import Data.Maybe
import Path
import Smos.API.Gen ()
import Smos.Sync.Client.Contents
import Smos.Sync.Client.ContentsMap
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.DirTree
import Smos.Sync.Client.DirTree.Gen
import Smos.Sync.Client.TestUtils
import Test.QuickCheck

hideFile :: Path Rel File -> Path Rel File
hideFile f = fromJust $ parseRelFile $ '.' : toFilePath f

hideDir :: Path Rel Dir -> Path Rel Dir
hideDir d = fromJust $ parseRelDir $ '.' : toFilePath d

genHiddenFile :: Gen (Path Rel File)
genHiddenFile =
  oneof
    [ do
        d1 <- genValid
        d2 <- genValid
        f <- genValid
        pure $ d1 </> hideDir d2 </> f,
      do
        d <- genValid
        f <- genValid
        pure $ d </> hideFile f
    ]

filterHiddenFiles :: ContentsMap -> ContentsMap
filterHiddenFiles = filterHiddenDirForest

sizedContentsMap :: Int -> Gen ContentsMap
sizedContentsMap 0 = pure CM.empty
sizedContentsMap i = scale (`div` i) $ go i
  where
    go 0 = pure CM.empty
    go j = do
      contentsMap <- go (j - 1)
      let ins cm = do
            path <- genValid
            contents <- genValid
            case CM.insert path contents cm of
              Nothing -> ins cm
              Just cm' -> pure cm'
      ins contentsMap

mapWithNewPath :: ContentsMap -> ByteString -> Gen (Path Rel File, ContentsMap)
mapWithNewPath = mapWithNewByGen genValid

mapWithNewHiddenPath :: ContentsMap -> ByteString -> Gen (Path Rel File, ContentsMap)
mapWithNewHiddenPath = mapWithNewByGen genHiddenFile

mapWithNewByGen ::
  Gen (Path Rel File) -> ContentsMap -> ByteString -> Gen (Path Rel File, ContentsMap)
mapWithNewByGen gen cm bs = gen `suchThatMap` (\p -> (,) p <$> CM.insert p bs cm)

mapsWithDifferentContentsAtNewPath :: ContentsMap -> Gen (ContentsMap, ContentsMap)
mapsWithDifferentContentsAtNewPath cm = do
  contents1 <- genValid
  contents2 <- genValid `suchThat` (/= contents1)
  genValid `suchThatMap` (\p -> (,) <$> CM.insert p contents1 cm <*> CM.insert p contents2 cm)

mapsWithDifferentContentsAtNewPath3 :: ContentsMap -> Gen (ContentsMap, ContentsMap, ContentsMap)
mapsWithDifferentContentsAtNewPath3 cm = do
  contents1 <- genValid
  contents2 <- genValid `suchThat` (/= contents1)
  contents3 <- genValid `suchThat` (/= contents1) `suchThat` (/= contents2)
  genValid
    `suchThatMap` ( \p ->
                      (,,) <$> CM.insert p contents1 cm <*> CM.insert p contents2 cm <*> CM.insert p contents3 cm
                  )

changedContentsMap :: ContentsMap -> Gen ContentsMap
changedContentsMap = changedDirForest

changedMapsWithUnionOf :: ContentsMap -> Gen (ContentsMap, ContentsMap)
changedMapsWithUnionOf cm =
  do
    cm1 <- genValid
    cm2 <- changedContentsMap cm1
    pure (CM.union cm1 cm, CM.union cm2 cm)

mapWithAdditions :: ContentsMap -> Gen ContentsMap
mapWithAdditions cm = CM.union cm <$> genValid

mapWithHiddenAdditions :: ContentsMap -> Gen ContentsMap
mapWithHiddenAdditions cm =
  (CM.union cm <$> ((genListOf1 ((,) <$> genHiddenFile <*> genValid)) `suchThatMap` CM.fromList))

-- Not ideal, but oh well
genListOf1 :: Gen a -> Gen [a]
genListOf1 g = (:) <$> g <*> genListOf g

twoDistinctPathsThatFitAndTheirUnion ::
  ByteString -> ByteString -> Gen (Path Rel File, Path Rel File, ContentsMap)
twoDistinctPathsThatFitAndTheirUnion contents1 contents2 = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionFunc
  pure (rp1, rp2, func contents1 contents2)

twoDistinctPathsThatFitAndTheirUnionFunc ::
  Gen (Path Rel File, Path Rel File, Hidden (ByteString -> ByteString -> ContentsMap))
twoDistinctPathsThatFitAndTheirUnionFunc = twoDistinctPathsThatFitAndTheirUnionWithFunc CM.empty

twoDistinctPathsThatFitAndTheirUnionWith ::
  ContentsMap -> ByteString -> ByteString -> Gen (Path Rel File, Path Rel File, ContentsMap)
twoDistinctPathsThatFitAndTheirUnionWith m contents1 contents2 = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionWithFunc m
  pure (rp1, rp2, func contents1 contents2)

twoDistinctPathsThatFitAndTheirUnionWithFunc ::
  ContentsMap ->
  Gen (Path Rel File, Path Rel File, Hidden (ByteString -> ByteString -> ContentsMap))
twoDistinctPathsThatFitAndTheirUnionWithFunc cm = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionsWithFunc cm
  let func' contents1 contents2 =
        let (_, _, m) = func contents1 contents2
         in m
  pure (rp1, rp2, Hidden func')

twoDistinctPathsThatFitAndTheirUnionsWith ::
  ContentsMap ->
  ByteString ->
  ByteString ->
  Gen (Path Rel File, Path Rel File, (ContentsMap, ContentsMap, ContentsMap))
twoDistinctPathsThatFitAndTheirUnionsWith cm contents1 contents2 = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionsWithFunc cm
  pure (rp1, rp2, func contents1 contents2)

twoDistinctPathsThatFitAndTheirUnionsWithFunc ::
  ContentsMap ->
  Gen
    ( Path Rel File,
      Path Rel File,
      Hidden (ByteString -> ByteString -> (ContentsMap, ContentsMap, ContentsMap))
    )
twoDistinctPathsThatFitAndTheirUnionsWithFunc cm = do
  rp1 <- genValid `suchThat` (\rp -> isJust $ CM.insert rp "" cm)
  rp2 <-
    genValid `suchThat` (/= rp1)
      `suchThat` (\rp -> isJust $ CM.insert rp1 "" cm >>= CM.insert rp "")
  pure
    ( rp1,
      rp2,
      Hidden $ \contents1 contents2 ->
        fromJust $
          (,,) <$> CM.insert rp1 contents1 cm
            <*> CM.insert rp2 contents2 cm
            <*> pure
              ( CM.unions
                  [ CM.singleton rp1 contents1,
                    CM.singleton rp2 contents2,
                    cm
                  ]
              )
    )

disjunctContentsMap :: ContentsMap -> Gen ContentsMap
disjunctContentsMap = disjunctDirForest

mapWithDisjunctUnion :: ContentsMap -> Gen (ContentsMap, ContentsMap)
mapWithDisjunctUnion cm = do
  cm' <- disjunctContentsMap cm
  pure (cm, CM.union cm cm')

twoChangedMapsAndTheirUnions ::
  Gen
    ( ( ContentsMap,
        -- m1
        ContentsMap
        -- m2
      ),
      ( ContentsMap,
        -- m1' = m3
        ContentsMap
        -- m2' = m4
      ),
      ( ContentsMap,
        -- m12
        ContentsMap,
        -- m14
        ContentsMap,
        -- m23
        ContentsMap
        -- m34
      )
    )
twoChangedMapsAndTheirUnions = twoChangedMapsAndTheirUnionsWith CM.empty

twoChangedMapsAndTheirUnionsWith ::
  ContentsMap ->
  Gen
    ( ( ContentsMap,
        -- m1
        ContentsMap
        -- m2
      ),
      ( ContentsMap,
        -- m1' = m3
        ContentsMap
        -- m2' = m4
      ),
      ( ContentsMap,
        -- m12
        ContentsMap,
        -- m14
        ContentsMap,
        -- m23
        ContentsMap
        -- m34
      )
    )
twoChangedMapsAndTheirUnionsWith cm = do
  cm1 <- genValid
  cm2 <- genValid
  let cm12 = CM.unions [cm1, cm2, cm]
  cm3 <-
    changedContentsMap cm1
  cm4 <-
    changedContentsMap cm2
  let cm14 = CM.unions [cm1, cm4, cm]
  let cm23 = CM.unions [cm2, cm3, cm]
  let cm34 = CM.unions [cm3, cm4, cm]
  pure ((cm1, cm2), (cm3, cm4), (cm12, cm14, cm23, cm34))

threeDisjunctMapsAndTheirUnions ::
  Gen
    ( ( ContentsMap,
        -- m1
        ContentsMap,
        -- m2
        ContentsMap
      ),
      -- m3)
      ( ContentsMap,
        -- m1 U m2
        ContentsMap,
        -- m2 U m3
        ContentsMap,
        -- m1 U m3
        ContentsMap
        -- m1 U m2 U m3
      )
    )
threeDisjunctMapsAndTheirUnions = do
  cm1 <- genValid
  cm2 <- disjunctContentsMap cm1
  let cm12 = CM.union cm1 cm2
  cm3 <-
    disjunctContentsMap cm12
  let cm23 = CM.union cm2 cm3
  let cm13 = CM.union cm1 cm3
  let cm123 = CM.unions [cm1, cm2, cm3]
  pure ((cm1, cm2, cm3), (cm12, cm23, cm13, cm123))

newtype Hidden a
  = Hidden a
  deriving (Eq, Ord)

instance Show (Hidden a) where
  show _ = "hidden"
