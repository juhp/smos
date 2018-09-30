{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw
    ( smosDraw
    ) where

import Import hiding ((<+>))

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Data.Tuple

import Brick.Types as B
import Brick.Widgets.Border as B
import Brick.Widgets.Center as B
import Brick.Widgets.Core as B
import Brick.Widgets.Core ((<+>))
import Graphics.Vty.Input.Events (Key(..), Modifier(..))

import Lens.Micro

import Cursor.FuzzyDay
import Cursor.Map
import Cursor.Simple.List.NonEmpty
import Cursor.Text
import Cursor.TextField
import Cursor.Tree hiding (drawTreeCursor)

import Smos.Data

import Smos.Cursor.Collapse
import Smos.Cursor.Contents
import Smos.Cursor.Editor
import Smos.Cursor.Entry
import Smos.Cursor.Header
import Smos.Cursor.Logbook
import Smos.Cursor.Properties
import Smos.Cursor.SmosFile
import Smos.Cursor.StateHistory
import Smos.Cursor.Tags
import Smos.Cursor.Timestamps

import Smos.Draw.Cursor
import Smos.Style
import Smos.Types

smosDraw :: SmosConfig -> SmosState -> [Widget ResourceName]
smosDraw SmosConfig {..} ss@SmosState {..} =
    [ centerLayer drawContextualHelpPage
    | editorCursorSelection smosStateCursor == HelpSelected
    ] ++
    [ vBox $
      concat
          [ [ maybe drawNoContent renderCursor $
              editorCursorFileCursor smosStateCursor
            ]
          , [drawDebug ss | editorCursorDebug smosStateCursor]
          ]
    ]
  where
    renderCursor :: SmosFileCursor -> Widget ResourceName
    renderCursor = viewport "viewport" Vertical . drawSmosFileCursor
    drawNoContent :: Widget n
    drawNoContent = B.vCenterLayer $ B.vBox [drawInfo, drawEmptyHelpPage]
      where
        drawEmptyHelpPage :: Widget n
        drawEmptyHelpPage =
            vBox $
            map
                (\(n, km) ->
                     padBottom (Pad 1) $
                     vBox
                         [ hCenterLayer $ withAttr selectedAttr $ str n
                         , hCenterLayer $ drawKeyMapHelp km
                         ])
                keyMaps
    drawContextualHelpPage :: Widget n
    drawContextualHelpPage =
        case smosStateCursor ^. editorCursorSmosFileCursorL of
            Nothing -> pageFor "Empty file" keyMapEmptyMatchers
            Just sfc ->
                case sfc ^. smosFileCursorEntrySelectionL of
                    WholeEntrySelected -> pageFor "Entry" keyMapEntryMatchers
                    HeaderSelected -> pageFor "Header" keyMapHeaderMatchers
                    ContentsSelected ->
                        pageFor "Contents" keyMapContentsMatchers
                    TimestampsSelected ->
                        pageFor "Timestamps" keyMapTimestampsMatchers
                    PropertiesSelected ->
                        pageFor "Properties" keyMapPropertiesMatchers
                    StateHistorySelected ->
                        pageFor "State History" keyMapStateHistoryMatchers
                    TagsSelected -> pageFor "Tags" keyMapTagsMatchers
                    LogbookSelected -> pageFor "Logbook" keyMapLogbookMatchers
      where
        pageFor :: String -> (KeyMap -> KeyMappings) -> Widget n
        pageFor s bindings =
            borderWithLabel (withAttr selectedAttr $ str ("[" ++ s ++ "]")) $
            padAll 1 $ drawKeyMapHelp bindings
    drawKeyMapHelp :: (KeyMap -> KeyMappings) -> Widget n
    drawKeyMapHelp m =
        drawTable $
        flip map (m configKeyMap) $ \km ->
            (drawKeyMappingEvent km, txt (keyMappingActionName km))
    keyMaps :: [(String, KeyMap -> KeyMappings)]
    keyMaps =
        [ ("Empty file", keyMapEmptyMatchers)
        , ("Entry", keyMapEntryMatchers)
        , ("Header", keyMapHeaderMatchers)
        , ("Contents", keyMapContentsMatchers)
        , ("Timestamps", keyMapTimestampsMatchers)
        , ("Properties", keyMapPropertiesMatchers)
        , ("State History", keyMapStateHistoryMatchers)
        , ("Tags", keyMapTagsMatchers)
        , ("Logbook", keyMapLogbookMatchers)
        , ("Help", keyMapHelpMatchers)
        ]
    drawInfo :: Widget n
    drawInfo =
        withAttr selectedAttr $
        vBox $
        map
            B.hCenterLayer
            [ str "SMOS"
            , str " "
            , str "version 0.0.0.0"
            , str "by Tom Sydney Kerckhove"
            , str "Smos is open source and freely distributable"
            , str " "
            , str " "
            ]

drawKeyMappingEvent :: KeyMapping -> Widget n
drawKeyMappingEvent (MapVtyExactly kp _) = str $ showKeypress kp
drawKeyMappingEvent (MapCatchAll _) = str "<any key>"
drawKeyMappingEvent (MapAnyTypeableChar _) = str "<any char>"
drawKeyMappingEvent (MapCombination kp km) =
    hBox [str $ showKeypress kp, drawKeyMappingEvent km]

keyMappingActionName :: KeyMapping -> Text
keyMappingActionName (MapVtyExactly _ a) = actionName a
keyMappingActionName (MapCatchAll a) = actionName a
keyMappingActionName (MapAnyTypeableChar a) = actionUsingName a
keyMappingActionName (MapCombination _ km) = keyMappingActionName km

drawHistory :: Seq KeyPress -> Widget n
drawHistory = strWrap . unwords . map showKeypress . toList

drawTable :: [(Widget n, Widget n)] -> Widget n
drawTable ls = vBox (map fst ls) <+> str "   " <+> vBox (map snd ls)

showKeypress :: KeyPress -> String
showKeypress (KeyPress key mods) =
    case mods of
        [] -> showKey key
        _ -> intercalate "-" $ map showMod mods ++ [showKey key]

showKey :: Key -> String
showKey (KChar c) = [c]
showKey (KFun i) = "F" ++ show i
showKey (KEsc) = "Esc"
showKey k = show k

showMod :: Modifier -> String
showMod MShift = "S"
showMod MCtrl = "C"
showMod MMeta = "M"
showMod MAlt = "A"

drawDebug :: SmosState -> Widget n
drawDebug SmosState {..} =
    vBox
        [ str "Key history: " <+> drawHistory smosStateKeyHistory
        , str "Last match: " <+>
          fromMaybe
              emptyWidget
              (drawLastMatches (debugInfoLastMatches smosStateDebugInfo))
        , strWrap $ ppShow smosStateCursor
        ]

drawLastMatches :: Maybe (NonEmpty ActivationDebug) -> Maybe (Widget n)
drawLastMatches Nothing = Nothing
drawLastMatches (Just ts) = Just $ vBox $ map (strWrap . ppShow) $ NE.toList ts

data Select
    = MaybeSelected
    | NotSelected

instance Semigroup Select where
    MaybeSelected <> MaybeSelected = MaybeSelected
    _ <> _ = NotSelected

defaultPadding :: Padding
defaultPadding = Pad 2

drawSmosFileCursor :: SmosFileCursor -> Widget ResourceName
drawSmosFileCursor =
    drawVerticalForestCursor drawEntryCTree drawSmosTreeCursor drawEntryCTree

drawSmosTreeCursor ::
       TreeCursor (CollapseEntry EntryCursor) (CollapseEntry Entry)
    -> Widget ResourceName
drawSmosTreeCursor = drawTreeCursor wrap cur
  where
    cur :: CollapseEntry EntryCursor
        -> CForest (CollapseEntry Entry)
        -> Widget ResourceName
    cur ec cf =
        case cf of
            EmptyCForest -> drawEntryCursor TreeIsNotCollapsed ec
            ClosedForest _ -> drawEntryCursor TreeIsCollapsed ec
            OpenForest ts ->
                drawEntryCursor TreeIsNotCollapsed ec <=>
                padLeft
                    defaultPadding
                    (vBox (map drawEntryCTree $ NE.toList ts))
    wrap ::
           [CTree (CollapseEntry Entry)]
        -> CollapseEntry Entry
        -> [CTree (CollapseEntry Entry)]
        -> Widget ResourceName
        -> Widget ResourceName
    wrap tsl e tsr w =
        drawEntry TreeIsNotCollapsed e <=>
        padLeft
            defaultPadding
            (vBox $ concat [map drawEntryCTree tsl, [w], map drawEntryCTree tsr])

drawEntryCTree :: CTree (CollapseEntry Entry) -> Widget ResourceName
drawEntryCTree (CNode t cf) =
    case cf of
        EmptyCForest -> drawEntry TreeIsNotCollapsed t
        ClosedForest _ -> drawEntry TreeIsCollapsed t
        OpenForest ts ->
            drawEntry TreeIsNotCollapsed t <=>
            padLeft defaultPadding (vBox (map drawEntryCTree $ NE.toList ts))

data TreeCollapsing
    = TreeIsNotCollapsed
    | TreeIsCollapsed
    deriving (Show, Eq)

drawEntryCursor ::
       TreeCollapsing -> CollapseEntry EntryCursor -> Widget ResourceName
drawEntryCursor tc e =
    vBox $
    catMaybes
        [ Just $
          hBox $
          intersperse (str " ") $
          concat $
          [ [selelectIfSelected $ str ">"]
          , maybeToList
                (entryCursorStateHistoryCursor >>= drawCurrentStateFromCursor)
          , [ drawHeaderCursor
                  (selectWhen HeaderSelected)
                  entryCursorHeaderCursor
            ]
          , maybeToList $
            drawTagsCursor (selectWhen TagsSelected) <$> entryCursorTagsCursor
          , [ str "..."
            | let e_ = rebuildEntryCursor ec
               in or [ not (collapseEntryShowContents e) &&
                       not (isNothing $ entryContents e_)
                     , not (collapseEntryShowHistory e) &&
                       not (nullStateHistory $ entryStateHistory e_)
                     , not (collapseEntryShowLogbook e) &&
                       not (nullLogbook $ entryLogbook e_)
                      ]
            ]
          , [str "+++" | tc == TreeIsCollapsed]
          ]
        , drawIfM collapseEntryShowContents $
          drawContentsCursor (selectWhen ContentsSelected) <$>
          entryCursorContentsCursor
        , drawTimestampsCursor (selectWhen TimestampsSelected) <$>
          entryCursorTimestampsCursor
        , drawPropertiesCursor (selectWhen PropertiesSelected) <$>
          entryCursorPropertiesCursor
        , drawIfM collapseEntryShowHistory $
          entryCursorStateHistoryCursor >>=
          drawStateHistoryCursor (selectWhen StateHistorySelected)
        , drawIfM collapseEntryShowLogbook $
          drawLogbookCursor
              (selectWhen LogbookSelected)
              entryCursorLogbookCursor
        ]
  where
    ec@EntryCursor {..} = collapseEntryValue e
    drawIfM :: (forall e. CollapseEntry e -> Bool) -> Maybe a -> Maybe a
    drawIfM bf mw = mw >>= drawIf bf
    drawIf :: (forall e. CollapseEntry e -> Bool) -> a -> Maybe a
    drawIf bf w =
        if bf e
            then Just w
            else Nothing
    selelectIfSelected :: Widget n -> Widget n
    selelectIfSelected =
        case selectWhen WholeEntrySelected of
            MaybeSelected -> withAttr selectedAttr . visible
            NotSelected -> id
    selectWhen :: EntryCursorSelection -> Select
    selectWhen ecs =
        if ecs == entryCursorSelected
            then MaybeSelected
            else NotSelected

drawEntry :: TreeCollapsing -> CollapseEntry Entry -> Widget ResourceName
drawEntry tc e =
    vBox $
    catMaybes
        [ Just $
          hBox $
          intersperse (str " ") $
          concat
              [ [str ">"]
              , maybeToList (drawCurrentState entryStateHistory)
              , [drawHeader entryHeader]
              , maybeToList (drawTags entryTags)
              , [ str "..."
                | or [ not (collapseEntryShowContents e) &&
                       not (isNothing entryContents)
                     , not (collapseEntryShowHistory e) &&
                       not (nullStateHistory entryStateHistory)
                     , not (collapseEntryShowLogbook e) &&
                       not (nullLogbook entryLogbook)
                      ]
                ]
              , [str "+++" | tc == TreeIsCollapsed]
              ]
        , drawIfM collapseEntryShowContents $ drawContents <$> entryContents
        , drawTimestamps entryTimestamps
        , drawProperties entryProperties
        , drawIfM collapseEntryShowHistory $ drawStateHistory entryStateHistory
        , drawIfM collapseEntryShowLogbook $ drawLogbook entryLogbook
        ]
  where
    Entry {..} = collapseEntryValue e
    drawIfM :: (forall e. CollapseEntry e -> Bool) -> Maybe a -> Maybe a
    drawIfM bf mw = mw >>= drawIf bf
    drawIf :: (forall e. CollapseEntry e -> Bool) -> a -> Maybe a
    drawIf bf w =
        if bf e
            then Just w
            else Nothing

drawHeaderCursor :: Select -> HeaderCursor -> Widget ResourceName
drawHeaderCursor s = withAttr headerAttr . drawTextCursor s

drawHeader :: Header -> Widget ResourceName
drawHeader = withAttr headerAttr . drawText . headerText

drawCurrentStateFromCursor :: StateHistoryCursor -> Maybe (Widget ResourceName)
drawCurrentStateFromCursor = drawCurrentState . rebuildStateHistoryCursor . Just

drawCurrentState :: StateHistory -> Maybe (Widget ResourceName)
drawCurrentState stateHistory =
    stateHistoryState stateHistory <&> \ts ->
        withAttr todoStateAttr $ drawTodoState ts

drawContentsCursor :: Select -> ContentsCursor -> Widget ResourceName
drawContentsCursor = drawTextFieldCursor

drawContents :: Contents -> Widget ResourceName
drawContents = drawText . contentsText

drawTimestampsCursor :: Select -> TimestampsCursor -> Widget ResourceName
drawTimestampsCursor s =
    drawVerticalMapCursor drawTimestamp (drawTimestampKVCursor s) drawTimestamp

drawTimestamps :: Map TimestampName Timestamp -> Maybe (Widget n)
drawTimestamps m
    | M.null m = Nothing
    | otherwise = Just $ vBox $ map (uncurry drawTimestamp) $ M.toList m

drawTimestampKVCursor ::
       Select
    -> KeyValueCursor TextCursor FuzzyDayCursor TimestampName Timestamp
    -> Widget ResourceName
drawTimestampKVCursor s kvc =
    case kvc of
        KeyValueCursorKey tc ts ->
            hBox
                [ case s of
                      NotSelected ->
                          drawTimestampName $ rebuildTimestampNameCursor tc
                      MaybeSelected -> drawTextCursor s tc
                , str ": "
                , str $ show $ timestampDay ts
                ]
        KeyValueCursorValue tsn fdc ->
            hBox
                [ drawTimestampName tsn
                , str ": "
                , case s of
                      NotSelected ->
                          str $ show $ timestampDay $ rebuildTimestampCursor fdc
                      MaybeSelected -> drawFuzzyDayCursor s fdc
                ]

drawTimestamp :: TimestampName -> Timestamp -> Widget n
drawTimestamp tsn d =
    hBox [drawTimestampName tsn, str ": ", str $ show $ timestampDay d]

drawFuzzyDayCursor :: Select -> FuzzyDayCursor -> Widget ResourceName
drawFuzzyDayCursor s fdc@FuzzyDayCursor {..} =
    hBox $
    intersperse (str " ") $
    [drawTextCursor s fuzzyDayCursorTextCursor] ++
    [ str "(" <+> str (show $ rebuildFuzzyDayCursor fdc) <+> str ")"
    | MaybeSelected <- [s]
    ]

drawTimestampName :: TimestampName -> Widget n
drawTimestampName tsn =
    withAttr (timestampNameSpecificAttr tsn <> timestampNameAttr) . txt $
    timestampNameText tsn

drawPropertiesCursor :: Select -> PropertiesCursor -> Widget ResourceName
drawPropertiesCursor _ = strWrap . show

drawProperties :: Map PropertyName PropertyValue -> Maybe (Widget ResourceName)
drawProperties m
    | M.null m = Nothing
    | otherwise = Just $ strWrap $ show m

drawStateHistoryCursor ::
       Select -> StateHistoryCursor -> Maybe (Widget ResourceName)
drawStateHistoryCursor _ = drawStateHistory . rebuildStateHistoryCursor . Just

drawStateHistory :: StateHistory -> Maybe (Widget ResourceName)
drawStateHistory (StateHistory ls)
    | null ls = Nothing
    | otherwise =
        Just $
        withAttr todoStateHistoryAttr $
        vBox $
        flip map ls $ \StateHistoryEntry {..} ->
            hBox $
            catMaybes
                [ Just $ strWrap $ show stateHistoryEntryTimestamp
                , ((str " " <+>) . drawTodoState) <$> stateHistoryEntryNewState
                ]

drawTagsCursor :: Select -> TagsCursor -> Widget ResourceName
drawTagsCursor _ tc =
    str ":" <+>
    hBox
        (intersperse (str ":") (map drawTag $ NE.toList $ rebuildTagsCursor tc)) <+>
    str ":"

drawTags :: [Tag] -> Maybe (Widget ResourceName)
drawTags ts
    | null ts = Nothing
    | otherwise =
        Just $
        str ":" <+> hBox (intersperse (str ":") (map drawTag ts)) <+> str ":"

drawTag :: Tag -> Widget n
drawTag = txt . tagText

drawLogbookCursor :: Select -> LogbookCursor -> Maybe (Widget ResourceName)
drawLogbookCursor _ lbc =
    case lbc of
        LogbookCursorClosed Nothing -> Nothing
        LogbookCursorClosed (Just ne) ->
            Just $
            vBox $ map drawLogbookEntry (NE.toList $ rebuildNonEmptyCursor ne)
        LogbookCursorOpen u ne ->
            Just $
            vBox $
            hBox [str "CLOCK: ", drawLogbookTimestamp u] :
            map
                drawLogbookEntry
                (maybe [] (NE.toList . rebuildNonEmptyCursor) ne)

drawLogbook :: Logbook -> Maybe (Widget n)
drawLogbook (LogClosed ls)
    | null ls = Nothing
    | otherwise = Just $ vBox $ map drawLogbookEntry ls
drawLogbook (LogOpen u ls) =
    Just $
    vBox $
    hBox [str "CLOCK: ", drawLogbookTimestamp u] : map drawLogbookEntry ls

drawLogbookEntry :: LogbookEntry -> Widget n
drawLogbookEntry LogbookEntry {..} =
    hBox
        [ str "CLOCK: "
        , drawLogbookTimestamp logbookEntryStart
        , str "--"
        , drawLogbookTimestamp logbookEntryEnd
        ]

drawLogbookTimestamp :: UTCTime -> Widget n
drawLogbookTimestamp utct =
    str "[" <+>
    str (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utct) <+> str "]"

drawTextCursor :: Select -> TextCursor -> Widget ResourceName
drawTextCursor s tc =
    (case s of
         MaybeSelected ->
             visible .
             showCursor textCursorName (B.Location (textCursorIndex tc, 0))
         _ -> id) $
    drawText $ rebuildTextCursor tc

drawTextFieldCursor :: Select -> TextFieldCursor -> Widget ResourceName
drawTextFieldCursor s tfc =
    (case s of
         MaybeSelected ->
             visible .
             showCursor
                 textCursorName
                 (B.Location (swap (textFieldCursorSelection tfc)))
         _ -> id) $
    drawText $ rebuildTextFieldCursor tfc

drawTodoState :: TodoState -> Widget ResourceName
drawTodoState ts =
    withAttr (todoStateSpecificAttr ts <> todoStateAttr) . txt $
    todoStateText ts

drawText :: Text -> Widget n
drawText = vBox . map go . T.splitOn "\n"
  where
    go t =
        txtWrap $
        case t of
            "" -> " "
            _ -> t
