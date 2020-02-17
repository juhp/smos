module Smos.Sync.Client.Sync.MetaSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity

import Smos.Sync.Client.Meta
import Smos.Sync.Client.MetaMap.Gen ()

spec :: Spec
spec =
  describe "makeClientMetaData" $
  it "produces valid meta maps" $ producesValidsOnValids2 makeClientMetaData
