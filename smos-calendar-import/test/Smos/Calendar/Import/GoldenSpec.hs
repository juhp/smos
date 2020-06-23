module Smos.Calendar.Import.GoldenSpec
  ( spec,
  )
where

import Control.Monad
import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Path
import Path.IO
import Smos.Calendar.Import
import Smos.Data
import Test.Hspec
import Text.ICalendar.Parser
import Text.Show.Pretty
import YamlParse.Applicative

spec :: Spec
spec = do
  fs <- runIO $ do
    testResourcesDir <- resolveDir' "test_resources"
    filter ((== ".ics") . fileExtension) . snd <$> listDirRecur testResourcesDir
  mapM_ mkGoldenTest fs

mkGoldenTest :: Path Abs File -> Spec
mkGoldenTest cp = it (fromAbsFile cp) $ do
  errOrCal <- parseICalendarFile def $ fromAbsFile cp
  case errOrCal of
    Left err -> expectationFailure $ unlines ["Failed to parse ical file: " <> fromAbsFile cp, err]
    Right (cals, _) -> do
      tcp <- setFileExtension ".yaml" cp
      mTestConf <- readConfigFile tcp
      case mTestConf of
        Nothing -> expectationFailure $ "Test conf not found: " <> fromAbsFile tcp
        Just pc -> do
          let actual = processCalendars pc cals
          sfp <- setFileExtension ".smos" cp
          mErrOrSmosFile <- readSmosFile sfp
          case mErrOrSmosFile of
            Nothing ->
              expectationFailure $
                unlines
                  ["Golden result not found: ", saveSuggestion sfp actual]
            Just errOrSmosFile -> case errOrSmosFile of
              Left err -> expectationFailure $ "Failed to parse smos file: " <> err
              Right expected ->
                unless (expected == actual)
                  $ expectationFailure
                  $ unlines
                    [ "expected: ",
                      ppShow expected,
                      "actual:",
                      ppShow actual,
                      saveSuggestion sfp actual
                    ]

saveSuggestion :: Path Abs File -> SmosFile -> String
saveSuggestion sfp sf =
  unlines
    [ fromAbsFile sfp,
      T.unpack (TE.decodeUtf8 (smosFileYamlBS sf))
    ]
