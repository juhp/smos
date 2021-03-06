{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Agenda where

import Conduit
import Control.DeepSeq
import Data.Aeson
import qualified Data.Conduit.Combinators as C
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Report.Agenda.Types
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Period
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import Smos.Report.TimeBlock

produceAgendaReport :: MonadIO m => ZonedTime -> Period -> TimeBlock -> HideArchive -> AgendaHistoricity -> Maybe EntryFilterRel -> DirectoryConfig -> m AgendaReport
produceAgendaReport now period timeBlock ha h f dc = do
  wd <- liftIO $ resolveDirWorkflowDir dc
  runConduit $
    streamSmosFilesFromWorkflowRel ha dc
      .| produceAgendaReportFromFiles now period timeBlock h f wd

produceAgendaReportFromFiles :: MonadIO m => ZonedTime -> Period -> TimeBlock -> AgendaHistoricity -> Maybe EntryFilterRel -> Path Abs Dir -> ConduitT (Path Rel File) void m AgendaReport
produceAgendaReportFromFiles now p tb h f wd = do
  filterSmosFilesRel
    .| parseSmosFilesRel wd
    .| printShouldPrint PrintWarning
    .| agendaReportConduit now p tb h f

agendaReportConduit :: Monad m => ZonedTime -> Period -> TimeBlock -> AgendaHistoricity -> Maybe EntryFilterRel -> ConduitT (Path Rel File, SmosFile) void m AgendaReport
agendaReportConduit now p tb h f =
  makeAgendaReport now p tb
    <$> ( smosFileCursors .| smosMFilter f
            .| smosCursorCurrents
            .| C.concatMap (uncurry makeAgendaEntry)
            .| C.filter (fitsHistoricity now h)
            .| sinkList
        )

data AgendaReport
  = AgendaReport
      { agendaReportPast :: [AgendaTableBlock Text],
        agendaReportPresent :: AgendaTodayReport,
        agendaReportFuture :: [AgendaTableBlock Text]
      }
  deriving (Show, Eq, Generic)

instance Validity AgendaReport

instance NFData AgendaReport

instance FromJSON AgendaReport where
  parseJSON = withObject "AgendaReport" $ \o -> AgendaReport <$> o .: "past" <*> o .: "present" <*> o .: "future"

instance ToJSON AgendaReport where
  toJSON AgendaReport {..} = object ["past" .= agendaReportPast, "present" .= agendaReportPresent, "future" .= agendaReportFuture]

makeAgendaReport :: ZonedTime -> Period -> TimeBlock -> [AgendaEntry] -> AgendaReport
makeAgendaReport now period tb as =
  let filteredAgenda = filter (filterPeriodLocal now period . timestampLocalTime . agendaEntryTimestamp) as
      (past, present, future) = divideIntoPastPresentFuture now filteredAgenda
      pastBlocks = divideIntoAgendaTableBlocks tb past
      futureBlocks = divideIntoAgendaTableBlocks tb future
   in AgendaReport
        { agendaReportPast = pastBlocks,
          agendaReportPresent =
            AgendaTodayReport
              { agendaTodayReportEntries = present
              },
          agendaReportFuture = futureBlocks
        }

divideIntoPastPresentFuture ::
  ZonedTime -> [AgendaEntry] -> ([AgendaEntry], [AgendaEntry], [AgendaEntry])
divideIntoPastPresentFuture now =
  splitList
    ( \ae ->
        compare (timestampDay $ agendaEntryTimestamp ae) (localDay $ zonedTimeToLocalTime now)
    )
    . sortAgendaEntries
  where
    splitList :: (a -> Ordering) -> [a] -> ([a], [a], [a])
    splitList func = go
      where
        go [] = ([], [], [])
        go (a : as) =
          case func a of
            LT ->
              case go as of
                (xs, ys, zs) -> (a : xs, ys, zs)
            EQ ->
              case go2 as of
                (ys, zs) -> ([], a : ys, zs)
            GT -> ([], [], a : as)
        go2 [] = ([], [])
        go2 (a : as) =
          case func a of
            LT -> error "should not happen"
            EQ ->
              case go2 as of
                (ys, zs) -> (a : ys, zs)
            GT -> ([], a : as)

newtype AgendaTodayReport
  = AgendaTodayReport
      { agendaTodayReportEntries :: [AgendaEntry]
      }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Semigroup, Monoid)

instance Validity AgendaTodayReport where
  validate atr@AgendaTodayReport {..} =
    mconcat
      [ genericValidate atr,
        declare "The entries are in chronological order" $
          agendaEntriesAreSorted agendaTodayReportEntries,
        declare "All the entries are in the same day"
          $ (<= 1)
          $ length
          $ group
          $ map (timestampDay . agendaEntryTimestamp) agendaTodayReportEntries
      ]

instance NFData AgendaTodayReport

type AgendaTableBlock a = Block a AgendaEntry

divideIntoAgendaTableBlocks :: TimeBlock -> [AgendaEntry] -> [AgendaTableBlock Text]
divideIntoAgendaTableBlocks = divideIntoBlocks (timestampDay . agendaEntryTimestamp)

agendaEntriesAreSorted :: [AgendaEntry] -> Bool
agendaEntriesAreSorted = areOrderedBy agendaEntrySortingProjection

areOrderedBy :: Eq a => (a -> a -> Ordering) -> [a] -> Bool
areOrderedBy func ls =
  sortBy func ls == ls

sortAgendaEntries :: [AgendaEntry] -> [AgendaEntry]
sortAgendaEntries =
  sortBy agendaEntrySortingProjection

agendaEntrySortingProjection :: AgendaEntry -> AgendaEntry -> Ordering
agendaEntrySortingProjection =
  mconcat
    [ comparing (timestampLocalTime . agendaEntryTimestamp),
      comparing (Down . agendaEntryTimestampName),
      comparing agendaEntryTodoState
    ]

data AgendaEntry
  = AgendaEntry
      { agendaEntryFilePath :: Path Rel File,
        agendaEntryHeader :: Header,
        agendaEntryTodoState :: Maybe TodoState,
        agendaEntryTimestampName :: TimestampName,
        agendaEntryTimestamp :: Timestamp
      }
  deriving (Show, Eq, Generic)

instance Validity AgendaEntry

instance NFData AgendaEntry

instance FromJSON AgendaEntry

instance ToJSON AgendaEntry

isDone :: Maybe TodoState -> Bool
isDone (Just "CANCELLED") = True
isDone (Just "DONE") = True
isDone (Just "FAILED") = True
isDone _ = False

makeAgendaEntry :: Path Rel File -> Entry -> [AgendaEntry]
makeAgendaEntry rp e =
  flip mapMaybe (M.toList $ entryTimestamps e) $ \(tsn, ts) ->
    if isDone (entryState e)
      then Nothing
      else
        Just
          AgendaEntry
            { agendaEntryFilePath = rp,
              agendaEntryHeader = entryHeader e,
              agendaEntryTodoState = entryState e,
              agendaEntryTimestampName = tsn,
              agendaEntryTimestamp = ts
            }

fitsHistoricity :: ZonedTime -> AgendaHistoricity -> AgendaEntry -> Bool
fitsHistoricity zt ah ae =
  case ah of
    HistoricalAgenda -> True
    FutureAgenda -> timestampLocalTime (agendaEntryTimestamp ae) >= zonedTimeToLocalTime zt
