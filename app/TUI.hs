{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (runSearchResultsTUI, runSearchResultsTUIWithAsync, runInteractiveTUI, MatchWithContext (..), SearchParams (..), ResultsAction (..), AppEvent (..)) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Focus qualified as F
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List
import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((&), (^.), (.~), (%~))
import Lens.Micro.Mtl (zoom)
import Lens.Micro.TH (makeLenses)
import Matcher
import System.Environment (lookupEnv)
import System.Process (callCommand)

-- | A match with file context
data MatchWithContext = MatchWithContext
  { _mwcMatch :: Match,
    _mwcFilePath :: FilePath,
    _mwcContextLines :: [(Int, String)]
  }

makeLenses ''MatchWithContext

-- | Search parameters to pass between TUI and Main
data SearchParams = SearchParams
  { spPattern :: String,
    spPath :: FilePath,
    spExtensions :: [String],
    spSearchTypes :: [SearchType]
  }
  deriving (Show)

-- | What action to take after viewing results
data ResultsAction
  = NewSearch
  | Quit
  deriving (Eq, Show)

-- | Custom events for async updates
data AppEvent
  = SearchComplete [MatchWithContext]  -- New search results arrived
  | UpdateProgress Int Int  -- parsed, total

-- | Resource names
data Name
  = MatchListName
  | PatternEditor
  | PathEditor
  | ExtensionEditor
  deriving (Eq, Ord, Show)

-- | Loading state
data LoadingState
  = NotLoading
  | Loading Int Int  -- files parsed, total files
  deriving (Eq)

-- | Application state - unified search and results
data AppState = AppState
  { _matchList :: List Name MatchWithContext,
    _patternEdit :: E.Editor Text Name,
    _pathEdit :: E.Editor Text Name,
    _extensionEdit :: E.Editor Text Name,
    _searchTypes'' :: [SearchType],
    _focusRing :: F.FocusRing Name,
    _searchFocused :: Bool,  -- Whether search input is focused
    _resultAction :: Maybe ResultsAction,
    _statusMessage :: Maybe String,  -- For showing status
    _loadingState :: LoadingState,  -- Loading progress
    _lastSearchParams :: Maybe (String, String, [String], [SearchType])  -- Last search parameters
  }

makeLenses ''AppState

-- | Initialize the app state with search params
initialState :: [MatchWithContext] -> String -> String -> [String] -> [SearchType] -> AppState
initialState matches pattern path exts types =
  AppState
    { _matchList = list MatchListName (Vec.fromList matches) 1,
      _patternEdit = E.editor PatternEditor (Just 1) (T.pack pattern),
      _pathEdit = E.editor PathEditor (Just 1) (T.pack path),
      _extensionEdit = E.editor ExtensionEditor (Just 1) (T.pack $ unwords exts),
      _searchTypes'' = types,
      _focusRing = F.focusRing [PatternEditor, PathEditor, ExtensionEditor],
      _searchFocused = False,
      _resultAction = Nothing,
      _statusMessage = Nothing,
      _loadingState = NotLoading,
      _lastSearchParams = Just (pattern, path, exts, types)  -- Store initial search params
    }

-- | Draw the UI - unified search and results
drawUI :: AppState -> [Widget Name]
drawUI st =
  case st ^. loadingState of
    Loading parsed total ->
      [drawLoadingOverlay parsed total, mainUI st]
    NotLoading ->
      [mainUI st]

mainUI :: AppState -> Widget Name
mainUI st = vBox
  [ drawHeader,
    hBorder,
    padLeftRight 1 $ drawSearchControls st,
    case st ^. statusMessage of
      Just msg -> padLeftRight 1 $ withAttr (attrName "status") $ str msg
      Nothing -> emptyWidget,
    hBorder,
    -- Match list and detail side-by-side
    hBox
      [ drawMatchList st,
        vBorder,
        drawMatchDetail st
      ],
    hBorder,
    drawHelp st
  ]

drawLoadingOverlay :: Int -> Int -> Widget Name
drawLoadingOverlay parsed total =
  centerLayer $
    withBorderStyle unicodeBold $
      border $
        padAll 2 $
          vBox
            [ withAttr (attrName "header") $ str "🔍 Searching..",
              str " "
            ]

drawProgressBar :: Int -> Int -> Widget Name
drawProgressBar parsed total =
  let width = 40
      progress = if total == 0 then 0 else (parsed * width) `div` total
      filled = replicate progress '█'
      empty = replicate (width - progress) '░'
   in str $ filled ++ empty

drawHeader :: Widget Name
drawHeader =
  withAttr (attrName "header") $
    padLeftRight 1 $
      str "fastgrep - AST-aware grep"

drawSearchControls :: AppState -> Widget Name
drawSearchControls st =
  if st ^. searchFocused
    then
      -- Expanded view when search is focused
      vBox
        [ hBox
            [ str "Pattern: ",
              hLimit 40 $ vLimit 1 $
                (if F.focusGetCurrent (st ^. focusRing) == Just PatternEditor
                 then withAttr (attrName "focused")
                 else id) $
                F.withFocusRing
                  (st ^. focusRing)
                  (E.renderEditor (txt . T.unlines))
                  (st ^. patternEdit)
            ],
          hBox
            [ str "  Path: ",
              hLimit 40 $ vLimit 1 $
                (if F.focusGetCurrent (st ^. focusRing) == Just PathEditor
                 then withAttr (attrName "focused")
                 else id) $
                F.withFocusRing
                  (st ^. focusRing)
                  (E.renderEditor (txt . T.unlines))
                  (st ^. pathEdit)
            ],
          hBox
            [ str "  Exts: ",
              hLimit 40 $ vLimit 1 $
                (if F.focusGetCurrent (st ^. focusRing) == Just ExtensionEditor
                 then withAttr (attrName "focused")
                 else id) $
                F.withFocusRing
                  (st ^. focusRing)
                  (E.renderEditor (txt . T.unlines))
                  (st ^. extensionEdit)
            ],
          hBox
            [ str "  Types: ",
              str $ "[" ++ (if Literal `elem` st ^. searchTypes'' then "X" else " ") ++ "] (l) Literal  ",
              str $ "[" ++ (if Identifier `elem` st ^. searchTypes'' then "X" else " ") ++ "] (i) Identifier  ",
              str $ "[" ++ (if FunctionCall `elem` st ^. searchTypes'' then "X" else " ") ++ "] (f) Function  ",
              str $ "[" ++ (if FunctionDef `elem` st ^. searchTypes'' then "X" else " ") ++ "] (d) Definition  ",
              str $ "[" ++ (if JsxText `elem` st ^. searchTypes'' then "X" else " ") ++ "] (x) JSX"
            ]
        ]
    else
      -- Collapsed view when browsing results (just show pattern on one line)
      hBox
        [ str "Pattern: ",
          str $ T.unpack (T.strip (T.unlines (E.getEditContents (st ^. patternEdit)))),
          str "  |  Types: ",
          str $ "[" ++ (if Literal `elem` st ^. searchTypes'' then "X" else " ") ++ "] (l) Literal  ",
          str $ "[" ++ (if Identifier `elem` st ^. searchTypes'' then "X" else " ") ++ "] (i) Identifier  ",
          str $ "[" ++ (if FunctionCall `elem` st ^. searchTypes'' then "X" else " ") ++ "] (f) Function  ",
          str $ "[" ++ (if FunctionDef `elem` st ^. searchTypes'' then "X" else " ") ++ "] (d) Definition  ",
          str $ "[" ++ (if JsxText `elem` st ^. searchTypes'' then "X" else " ") ++ "] (x) JSX",
          str "  (press 's' to edit)"
        ]

drawMatchList :: AppState -> Widget Name
drawMatchList st =
  let total = Vec.length (listElements (st ^. matchList))
      current = maybe 0 (+ 1) (listSelected (st ^. matchList))
   in hLimit 60 $
        vBox
          [ padLeftRight 1 $ str $ "Matches (" ++ show current ++ "/" ++ show total ++ ")",
            renderList renderMatch True (st ^. matchList)
          ]

renderMatch :: Bool -> MatchWithContext -> Widget Name
renderMatch isSelected mwc =
  let match = _mwcMatch mwc
      filepath = _mwcFilePath mwc
      typeLabel = getMatchTypeLabel (matchType match)
      locStr = filepath ++ ":" ++ show (matchLine match) ++ ":" ++ show (matchColumn match)
      style =
        if isSelected
          then withAttr (attrName "selected")
          else id
   in style $
        padLeftRight 1 $
          hBox
            [ withAttr (getMatchTypeAttr (matchType match)) $ str typeLabel,
              str " ",
              str locStr
            ]

drawMatchDetail :: AppState -> Widget Name
drawMatchDetail st =
  case listSelectedElement (st ^. matchList) of
    Nothing -> padLeftRight 1 $ str "No matches found"
    Just (_, mwc) ->
      let match = _mwcMatch mwc
          filepath = _mwcFilePath mwc
          contextLines = _mwcContextLines mwc
          matchLineNum = matchLine match
       in vBox
            [ padLeftRight 1 $
                withAttr (attrName "detail-header") $
                  str $
                    "Match: " ++ matchText match,
              padLeftRight 1 $
                str $
                  "Location: " ++ filepath ++ ":" ++ show matchLineNum ++ ":" ++ show (matchColumn match),
              hBorder,
              padLeft (Pad 2) $
                vBox $
                  map (drawContextLine matchLineNum match) contextLines
            ]

drawContextLine :: Int -> Match -> (Int, String) -> Widget Name
drawContextLine matchLineNum match (lineNum, lineText) =
  let isMatchLine = lineNum == matchLineNum
      lineNumStr = padLeft (Pad (4 - length (show lineNum))) $ str (show lineNum)
      lineWidget =
        if isMatchLine
          then withAttr (getMatchTypeAttr (matchType match)) $ str lineText
          else withAttr (attrName "context") $ str lineText
   in hBox
        [ if isMatchLine
            then withAttr (attrName "line-num-highlight") lineNumStr
            else withAttr (attrName "line-num") lineNumStr,
          str " │ ",
          lineWidget
        ]

drawHelp :: AppState -> Widget Name
drawHelp st =
  padLeftRight 1 $
    if st ^. searchFocused
      then str "Tab: next field | ESC: unfocus | Enter: search | l/i/f/d/x: toggle types"
      else str "s/n: focus search | ↑,j/↓,k: navigate | o: open in $EDITOR | l/i/f/d/x: toggle types | Enter: search | q: quit"

-- | Get match type label
getMatchTypeLabel :: SearchType -> String
getMatchTypeLabel Literal = "[LIT]"
getMatchTypeLabel Identifier = "[ID]"
getMatchTypeLabel FunctionCall = "[CALL]"
getMatchTypeLabel FunctionDef = "[DEF]"
getMatchTypeLabel JsxText = "[JSX]"

-- | Get match type attribute
getMatchTypeAttr :: SearchType -> AttrName
getMatchTypeAttr Literal = attrName "type-literal"
getMatchTypeAttr Identifier = attrName "type-identifier"
getMatchTypeAttr FunctionCall = attrName "type-function-call"
getMatchTypeAttr FunctionDef = attrName "type-function-def"
getMatchTypeAttr JsxText = attrName "type-jsx-text"

-- | Handle events
handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent (VtyEvent e) = do
  st <- get
  if st ^. searchFocused
    then handleSearchModeEvent e  -- Search input is focused
    else handleBrowseModeEvent e  -- Browsing results
handleEvent (AppEvent (SearchComplete matches)) = do
  -- Update results when search completes
  modify $ \st -> st
    & matchList .~ list MatchListName (Vec.fromList matches) 1
    & loadingState .~ NotLoading
handleEvent (AppEvent (UpdateProgress parsed total)) = do
  -- Update progress bar
  modify $ \st -> st & loadingState .~ Loading parsed total
handleEvent _ = return ()

-- | Handle events when search is focused
handleSearchModeEvent :: V.Event -> EventM Name AppState ()
handleSearchModeEvent e =
  case e of
    V.EvKey V.KEsc [] -> modify $ \st -> st & searchFocused .~ False
    V.EvKey (V.KChar '\t') [] -> modify $ \st -> st & focusRing %~ F.focusNext
    V.EvKey V.KBackTab [] -> modify $ \st -> st & focusRing %~ F.focusPrev
    V.EvKey V.KEnter [] -> do
      st <- get
      let currentPattern = T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. patternEdit)
          currentPath = T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. pathEdit)
          currentExts = words $ T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. extensionEdit)
          currentTypes = st ^. searchTypes''
          currentParams = (currentPattern, currentPath, currentExts, currentTypes)
          hasChanged = case st ^. lastSearchParams of
            Nothing -> True
            Just lastParams -> currentParams /= lastParams
      if hasChanged
        then modify (\s -> s
          & resultAction .~ Just NewSearch
          & searchFocused .~ False
          & lastSearchParams .~ Just currentParams) >> halt
        else modify $ \s -> s & searchFocused .~ False  -- Just unfocus, don't search
    -- Toggle types still works while editing
    V.EvKey (V.KChar 'l') [V.MCtrl] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType Literal
    V.EvKey (V.KChar 'i') [V.MCtrl] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType Identifier
    V.EvKey (V.KChar 'f') [V.MCtrl] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType FunctionCall
    V.EvKey (V.KChar 'd') [V.MCtrl] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType FunctionDef
    V.EvKey (V.KChar 'x') [V.MCtrl] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType JsxText
    _ -> do
      st <- get
      case F.focusGetCurrent (st ^. focusRing) of
        Just PatternEditor -> zoom patternEdit $ E.handleEditorEvent (VtyEvent e)
        Just PathEditor -> zoom pathEdit $ E.handleEditorEvent (VtyEvent e)
        Just ExtensionEditor -> zoom extensionEdit $ E.handleEditorEvent (VtyEvent e)
        _ -> return ()

-- | Handle events when browsing results
handleBrowseModeEvent :: V.Event -> EventM Name AppState ()
handleBrowseModeEvent e =
  case e of
    V.EvKey (V.KChar 'q') [] -> modify (\s -> s & resultAction .~ Just Quit) >> halt
    V.EvKey V.KEsc [] -> modify (\s -> s & resultAction .~ Just Quit) >> halt
    V.EvKey (V.KChar 's') [] -> modify $ \st -> st & searchFocused .~ True
    V.EvKey (V.KChar 'n') [] -> modify $ \st -> st & searchFocused .~ True
    V.EvKey V.KEnter [] -> do
      st <- get
      let currentPattern = T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. patternEdit)
          currentPath = T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. pathEdit)
          currentExts = words $ T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. extensionEdit)
          currentTypes = st ^. searchTypes''
          currentParams = (currentPattern, currentPath, currentExts, currentTypes)
          hasChanged = case st ^. lastSearchParams of
            Nothing -> True
            Just lastParams -> currentParams /= lastParams
      when hasChanged $ do
        modify $ \s -> s
          & loadingState .~ Loading 0 1
          & resultAction .~ Just NewSearch
          & lastSearchParams .~ Just currentParams
        halt
    V.EvKey (V.KChar 'o') [] -> do
      st <- get
      case listSelectedElement (st ^. matchList) of
        Nothing -> return ()
        Just (_, mwc) -> suspendAndResume $ do
          openInEditor mwc
          return st
    -- Navigate results
    V.EvKey V.KDown [] -> modify $ \st -> st {_matchList = listMoveDown (_matchList st)}
    V.EvKey V.KUp [] -> modify $ \st -> st {_matchList = listMoveUp (_matchList st)}
    V.EvKey (V.KChar 'j') [] -> modify $ \st -> st {_matchList = listMoveDown (_matchList st)}
    V.EvKey (V.KChar 'k') [] -> modify $ \st -> st {_matchList = listMoveUp (_matchList st)}
    -- Toggle search types
    V.EvKey (V.KChar 'l') [] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType Literal
    V.EvKey (V.KChar 'i') [] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType Identifier
    V.EvKey (V.KChar 'f') [] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType FunctionCall
    V.EvKey (V.KChar 'd') [] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType FunctionDef
    V.EvKey (V.KChar 'x') [] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType JsxText
    _ -> return ()

-- | Open the selected match in $EDITOR
openInEditor :: MatchWithContext -> IO ()
openInEditor mwc = do
  let match = _mwcMatch mwc
      filepath = _mwcFilePath mwc
      lineNum = matchLine match

  maybeEditor <- lookupEnv "EDITOR"
  case maybeEditor of
    Nothing -> putStrLn "Error: $EDITOR not set. Cannot open file."
    Just editor -> do
      -- Most editors support +line syntax (vim, emacs, nano, etc.)
      let command = editor ++ " +" ++ show lineNum ++ " " ++ show filepath
      callCommand command

toggleSearchType :: SearchType -> [SearchType] -> [SearchType]
toggleSearchType t ts
  | t `elem` ts = filter (/= t) ts
  | otherwise = t : ts

-- | Attribute map
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (attrName "header", V.cyan `on` V.black `V.withStyle` V.bold),
      (attrName "selected", V.black `on` V.white),
      (attrName "focused", V.black `on` V.yellow),
      (attrName "status", fg V.yellow `V.withStyle` V.bold),
      (attrName "detail-header", fg V.yellow `V.withStyle` V.bold),
      (attrName "line-num", fg V.brightBlack),
      (attrName "line-num-highlight", fg V.yellow `V.withStyle` V.bold),
      (attrName "context", fg V.white),
      (attrName "type-literal", fg V.yellow `V.withStyle` V.bold),
      (attrName "type-identifier", fg V.cyan `V.withStyle` V.bold),
      (attrName "type-function-call", fg V.magenta `V.withStyle` V.bold),
      (attrName "type-function-def", fg V.green `V.withStyle` V.bold),
      (attrName "type-jsx-text", fg V.blue `V.withStyle` V.bold),
      (E.editAttr, V.white `on` V.black),
      (E.editFocusedAttr, V.black `on` V.white)
    ]

-- | The app definition
app :: App AppState AppEvent Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = F.focusRingCursor (^. focusRing),
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

-- | Run the TUI with search results, returns the action and updated search params
runSearchResultsTUI :: [MatchWithContext] -> String -> String -> [String] -> [SearchType] -> IO (ResultsAction, SearchParams)
runSearchResultsTUI matches pattern path exts types = do
  eventChan <- newBChan 10
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty (Just eventChan) app (initialState matches pattern path exts types)

  -- Extract search parameters from final state
  let patternText = T.unpack . T.concat $ E.getEditContents (finalState ^. patternEdit)
      pathText = T.unpack . T.concat $ E.getEditContents (finalState ^. pathEdit)
      extText = T.unpack . T.concat $ E.getEditContents (finalState ^. extensionEdit)
      types' = finalState ^. searchTypes''
      exts' = filter (not . null) $ words extText
      params = SearchParams
        { spPattern = if null patternText then pattern else patternText,
          spPath = if null pathText then "." else pathText,
          spExtensions = exts',
          spSearchTypes = types'
        }

  return (fromMaybe Quit (finalState ^. resultAction), params)

-- | Run the TUI with async search capability - starts with loading overlay
runSearchResultsTUIWithAsync :: [MatchWithContext] -> String -> String -> [String] -> [SearchType] -> (BChan AppEvent -> IO ()) -> IO (ResultsAction, SearchParams)
runSearchResultsTUIWithAsync previousMatches pattern path exts types searchAction = do
  eventChan <- newBChan 10
  let buildVty = mkVty V.defaultConfig

  -- Start async search in background
  _ <- forkIO $ searchAction eventChan

  initialVty <- buildVty
  let initState = (initialState previousMatches pattern path exts types) & loadingState .~ Loading 0 1
  finalState <- customMain initialVty buildVty (Just eventChan) app initState

  -- Extract search parameters from final state
  let patternText = T.unpack . T.concat $ E.getEditContents (finalState ^. patternEdit)
      pathText = T.unpack . T.concat $ E.getEditContents (finalState ^. pathEdit)
      extText = T.unpack . T.concat $ E.getEditContents (finalState ^. extensionEdit)
      types' = finalState ^. searchTypes''
      exts' = filter (not . null) $ words extText
      params = SearchParams
        { spPattern = if null patternText then pattern else patternText,
          spPath = if null pathText then "." else pathText,
          spExtensions = exts',
          spSearchTypes = types'
        }

  return (fromMaybe Quit (finalState ^. resultAction), params)

-- | Run interactive TUI - returns search parameters or Nothing if user quit
-- This is just the unified TUI with empty results and search focused
runInteractiveTUI :: a -> IO (Maybe SearchParams)
runInteractiveTUI _opts = do
  eventChan <- newBChan 10
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty
  -- Start with empty results, search focused
  let initState = (initialState [] "" "." [] []) & searchFocused .~ True
  finalState <- customMain initialVty buildVty (Just eventChan) app initState

  -- Extract search parameters
  let patternText = T.unpack . T.concat $ E.getEditContents (finalState ^. patternEdit)
      pathText = T.unpack . T.concat $ E.getEditContents (finalState ^. pathEdit)
      extText = T.unpack . T.concat $ E.getEditContents (finalState ^. extensionEdit)
      types = finalState ^. searchTypes''
      exts = filter (not . null) $ words extText
      action = finalState ^. resultAction

  -- Only return params if user triggered search
  case action of
    Just NewSearch | not (null patternText) ->
      return $ Just SearchParams
        { spPattern = patternText,
          spPath = if null pathText then "." else pathText,
          spExtensions = exts,
          spSearchTypes = types
        }
    _ -> return Nothing  -- User quit without searching
