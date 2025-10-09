{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (runSearchResultsTUI, runSearchResultsTUIWithAsync, runInteractiveTUI, MatchWithContext (..), SearchParams (..), ResultsAction (..), AppEvent (..)) where

import Brick
import Brick.BChan (BChan, newBChan)
import Brick.Focus qualified as F
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List
import Control.Concurrent (forkIO)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.String ()
import Control.Exception (try, evaluate, SomeException)
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((&), (^.), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import Matcher
import System.Environment (lookupEnv)
import System.IO (readFile, writeFile)
import System.Process (callCommand, readProcess)

-- | A match with file context
data MatchWithContext = MatchWithContext
  { _mwcMatch :: Match,
    _mwcFilePath :: FilePath,
    _mwcContextLines :: [(Int, String)]
  }


-- | Search parameters to pass between TUI and Main
data SearchParams = SearchParams
  { spPattern :: String,
    spPath :: FilePath,
    spExtensions :: [String],
    spSearchTypes :: [SearchType],
    spModifiedFiles :: [FilePath]  -- Files that were modified via replace
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
  | ReplaceEditor
  deriving (Eq, Ord, Show)

-- | Loading state
data LoadingState
  = NotLoading
  | LoadingCache Int Int  -- Building AST cache: files parsed, total files
  | SearchingCache  -- Searching through cached ASTs
  deriving (Eq)

-- | Application state - unified search and results
data AppState = AppState
  { _matchList :: List Name MatchWithContext,
    _patternEdit :: E.Editor Text Name,
    _pathEdit :: E.Editor Text Name,
    _extensionEdit :: E.Editor Text Name,
    _replaceEdit :: E.Editor Text Name,  -- Replace text input
    _searchTypes'' :: [SearchType],
    _focusRing :: F.FocusRing Name,
    _searchFocused :: Bool,  -- Whether search input is focused
    _replaceFocused :: Bool,  -- Whether replace input is focused
    _resultAction :: Maybe ResultsAction,
    _statusMessage :: Maybe String,  -- For showing status
    _loadingState :: LoadingState,  -- Loading progress
    _lastSearchParams :: Maybe (String, String, [String], [SearchType]),  -- Last search parameters
    _modifiedFiles :: [FilePath]  -- Files modified via replace operations
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
      _replaceEdit = E.editor ReplaceEditor (Just 1) "",
      _searchTypes'' = types,
      _focusRing = F.focusRing [PatternEditor, PathEditor, ExtensionEditor],
      _searchFocused = False,
      _replaceFocused = False,
      _resultAction = Nothing,
      _statusMessage = Nothing,
      _loadingState = NotLoading,
      _lastSearchParams = Just (pattern, path, exts, types),
      _modifiedFiles = []
    }

drawUI :: AppState -> [Widget Name]
drawUI st =
  let baseUI = case st ^. loadingState of
        LoadingCache parsed total ->
          [drawCacheBuildingOverlay parsed total, mainUI st]
        SearchingCache ->
          [drawSearchingOverlay, mainUI st]
        NotLoading ->
          [mainUI st]
   in if st ^. replaceFocused
        then drawReplaceDialog st : baseUI
        else baseUI

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

drawCacheBuildingOverlay :: Int -> Int -> Widget Name
drawCacheBuildingOverlay parsed total =
  centerLayer $
    withBorderStyle unicodeBold $
      border $
        padAll 2 $
          vBox
            [ withAttr (attrName "header") $ str "🔨 Building AST Cache",
              str " ",
              str $ "Parsing files: " ++ show parsed ++ " / " ++ show total,
              str " ",
              drawProgressBar parsed total,
              str " ",
              str "This happens once per codebase."
            ]

drawSearchingOverlay :: Widget Name
drawSearchingOverlay =
  centerLayer $
    withBorderStyle unicodeBold $
      border $
        padAll 2 $
          vBox
            [ withAttr (attrName "header") $ str "🔍 Searching..",
              str " "
            ]

drawReplaceDialog :: AppState -> Widget Name
drawReplaceDialog st =
  let searchPattern = T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. patternEdit)
      replaceText = T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. replaceEdit)
      (captureInfo, beforeText, afterText) = case listSelectedElement (st ^. matchList) of
        Nothing -> ([], "", "")
        Just (_, mwc) ->
          let match = _mwcMatch mwc
              matched = matchText match
           in case safeRegexMatch searchPattern matched of
                Nothing -> ([], matched, matched)  -- Invalid regex or no match, show original
                Just (before, fullMatch, after, captures) ->
                  let substituted = if null replaceText
                                      then fullMatch
                                      else substituteCaptures replaceText fullMatch captures
                      -- Show the entire node text with the replacement applied
                      afterComplete = before ++ substituted ++ after
                   in (captures, matched, afterComplete)
      captureWidget = if null captureInfo
                        then emptyWidget
                        else vBox
                          [ str " ",
                            str "Capture groups:",
                            str "  \\0 the full match",
                            vBox $ zipWith (\i cap -> str $ "  \\" ++ show i ++ " = " ++ cap) [1 :: Int ..] captureInfo,
                            str " "
                          ]
      previewWidget = if null afterText || beforeText == afterText
                        then emptyWidget
                        else vBox
                          [ str " ",
                            hBox
                              [ str "Preview: ",
                                withAttr (attrName "type-literal") $ str beforeText,
                                str " → ",
                                withAttr (attrName "type-function-call") $ str afterText
                              ],
                            str " "
                          ]
   in centerLayer $
        withBorderStyle unicodeBold $
          border $
            padAll 2 $
              vBox
                [ withAttr (attrName "header") $ str "Replace",
                  str " ",
                  hBox [ str "Replace with: ",
                         hLimit 50 $ vLimit 1 $
                           withAttr (attrName "focused") $
                           E.renderEditor (txt . T.unlines) True (st ^. replaceEdit)
                       ],
                  captureWidget,
                  previewWidget,
                  str " ",
                  str "Enter: replace current | Ctrl+A: replace all | ESC: cancel"
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
              str $ "[" ++ (if Literal `elem` st ^. searchTypes'' then "✓" else " ") ++ "] (l) Literal  ",
              str $ "[" ++ (if Identifier `elem` st ^. searchTypes'' then "✓" else " ") ++ "] (i) Identifier  ",
              str $ "[" ++ (if FunctionCall `elem` st ^. searchTypes'' then "✓" else " ") ++ "] (c) Call  ",
              str $ "[" ++ (if FunctionDef `elem` st ^. searchTypes'' then "✓" else " ") ++ "] (d) Definition  ",
              str $ "[" ++ (if JsxText `elem` st ^. searchTypes'' then "✓" else " ") ++ "] (t) Inner Text"
            ]
        ]
    else
      -- Collapsed view when browsing results (just show pattern on one line)
      hBox
        [
          str $ T.unpack (T.strip (T.unlines (E.getEditContents (st ^. patternEdit)))),
          str "  | ",
          str $ "[" ++ (if Literal `elem` st ^. searchTypes'' then "✓" else " ") ++ "] (l) Literal  ",
          str $ "[" ++ (if Identifier `elem` st ^. searchTypes'' then "✓" else " ") ++ "] (i) Identifier  ",
          str $ "[" ++ (if FunctionCall `elem` st ^. searchTypes'' then "✓" else " ") ++ "] (c) Call  ",
          str $ "[" ++ (if FunctionDef `elem` st ^. searchTypes'' then "✓" else " ") ++ "] (d) Definition  ",
          str $ "[" ++ (if JsxText `elem` st ^. searchTypes'' then "✓" else " ") ++ "] (t) Inner Text",
          str "  (press 'f' to edit)"
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
      _style =
        if isSelected
          then withAttr (attrName "selected")
          else id
   in _style $
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
          typeLabel = getMatchTypeLabel (matchType match)
          contextLines = _mwcContextLines mwc
          matchLineNum = matchLine match
          matchedText = matchText match
          -- Get the search pattern from the editor
          searchPattern = T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. patternEdit)
       in vBox
            [ padLeftRight 1 $
                withAttr (attrName "detail-header") $
                  str $
                    typeLabel,
              padLeftRight 1 $
                str $
                  "Location: " ++ filepath ++ ":" ++ show matchLineNum ++ ":" ++ show (matchColumn match),
              padLeftRight 1 $
                hBox
                  [ str "Match: ",
                    withAttr (getMatchTypeAttr (matchType match)) $ str matchedText
                  ],
              hBorder,
              padLeft (Pad 2) $
                vBox $
                  map (drawContextLine matchLineNum match searchPattern) contextLines
            ]

drawContextLine :: Int -> Match -> String -> (Int, String) -> Widget Name
drawContextLine matchLineNum match searchPattern (lineNum, lineText) =
  let isMatchLine = lineNum == matchLineNum
      lineNumStr = padLeft (Pad (4 - length (show lineNum))) $ str (show lineNum)
      lineWidget =
        if isMatchLine
          then
            if null searchPattern
              then
                -- Empty pattern - just show the line normally
                withAttr (getMatchTypeAttr (matchType match)) $ str lineText
              else
                -- Highlight the search pattern within the line using regex
                case safeRegexMatch searchPattern lineText of
                  Nothing ->
                    -- Invalid regex or no match - fallback to highlighting the whole matched node
                    withAttr (getMatchTypeAttr (matchType match)) $ str lineText
                  Just (before, matched, after, _) ->
                    -- Highlight the matched portion
                    hBox
                      [ withAttr (attrName "context") $ str before,
                        withAttr (getMatchTypeAttr (matchType match)) $ str matched,
                        withAttr (attrName "context") $ str after
                      ]
          else withAttr (attrName "context") $ str lineText
   in hBox
        [ if isMatchLine
            then withAttr (attrName "line-num-highlight") lineNumStr
            else withAttr (attrName "line-num") lineNumStr,
          str " │ ",
          lineWidget
        ]


drawHelp :: AppState -> Widget Name
drawHelp appState =
  padLeftRight 1 $
    if appState ^. searchFocused
      then str "Tab: next field | ESC: unfocus | Enter: search | l/i/c/d/t: toggle types"
      else str "f/s/n: focus search | ↑,j/↓,k: navigate | y: copy | o: open | r: replace | l/i/c/d/t: toggle types | Enter: search | q: quit"

getMatchTypeLabel :: SearchType -> String
getMatchTypeLabel Literal = "[LIT]"
getMatchTypeLabel Identifier = "[ID]"
getMatchTypeLabel FunctionCall = "[CALL]"
getMatchTypeLabel FunctionDef = "[DEF]"
getMatchTypeLabel JsxText = "[TEXT]"

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
  if st ^. replaceFocused
    then handleReplaceModeEvent e  -- Replace input is focused
    else if st ^. searchFocused
      then handleSearchModeEvent e  -- Search input is focused
      else handleBrowseModeEvent e  -- Browsing results
handleEvent (AppEvent (SearchComplete matches)) = do
  -- Update results when search completes, preserving scroll position
  st <- get
  let oldList = st ^. matchList
      oldSelected = listSelected oldList
      newList = list MatchListName (Vec.fromList matches) 1
      -- Preserve the selection if it's still valid
      finalList = case oldSelected of
        Nothing -> newList
        Just idx -> if idx < length matches
                      then listMoveTo idx newList
                      else newList
  modify $ \_ -> st
    & matchList .~ finalList
    & loadingState .~ NotLoading
handleEvent (AppEvent (UpdateProgress parsed total)) = do
  -- Update progress bar for cache building
  modify $ \st -> st & loadingState .~ LoadingCache parsed total
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
    V.EvKey (V.KChar 'c') [V.MCtrl] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType FunctionCall
    V.EvKey (V.KChar 'd') [V.MCtrl] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType FunctionDef
    V.EvKey (V.KChar 't') [V.MCtrl] -> modify $ \st -> st & searchTypes'' %~ toggleSearchType JsxText
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
    V.EvKey (V.KChar 'f') [] -> modify $ \st -> st & searchFocused .~ True
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
          & loadingState .~ SearchingCache
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
    V.EvKey (V.KChar 'y') [] -> do
      st <- get
      case listSelectedElement (st ^. matchList) of
        Nothing -> return ()
        Just (_, mwc) -> suspendAndResume $ do
          copyToClipboard mwc
          return st
    V.EvKey (V.KChar 'r') [] -> modify $ \st -> st & replaceFocused .~ True
    V.EvKey V.KDown [] -> modify $ \st -> st {_matchList = listMoveDown (_matchList st)}
    V.EvKey V.KUp [] -> modify $ \st -> st {_matchList = listMoveUp (_matchList st)}
    V.EvKey (V.KChar 'j') [] -> modify $ \st -> st {_matchList = listMoveDown (_matchList st)}
    V.EvKey (V.KChar 'k') [] -> modify $ \st -> st {_matchList = listMoveUp (_matchList st)}
    V.EvKey (V.KChar 'l') [] -> do
      modify $ \st -> st & searchTypes'' %~ toggleSearchType Literal
                         & resultAction .~ Just NewSearch
      halt
    V.EvKey (V.KChar 'i') [] -> do
      modify $ \st -> st & searchTypes'' %~ toggleSearchType Identifier
                         & resultAction .~ Just NewSearch
      halt
    V.EvKey (V.KChar 'c') [] -> do
      modify $ \st -> st & searchTypes'' %~ toggleSearchType FunctionCall
                         & resultAction .~ Just NewSearch
      halt
    V.EvKey (V.KChar 'd') [] -> do
      modify $ \st -> st & searchTypes'' %~ toggleSearchType FunctionDef
                         & resultAction .~ Just NewSearch
      halt
    V.EvKey (V.KChar 't') [] -> do
      modify $ \st -> st & searchTypes'' %~ toggleSearchType JsxText
                         & resultAction .~ Just NewSearch
      halt
    _ -> return ()

-- | Handle events when replace dialog is open
handleReplaceModeEvent :: V.Event -> EventM Name AppState ()
handleReplaceModeEvent e =
  case e of
    V.EvKey V.KEsc [] -> modify $ \st -> st & replaceFocused .~ False & replaceEdit .~ E.editor ReplaceEditor (Just 1) ""
    V.EvKey V.KEnter [] -> do
      st <- get
      let replaceText = T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. replaceEdit)
          searchPattern = T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. patternEdit)
      case listSelectedElement (st ^. matchList) of
        Nothing -> return ()
        Just (_, mwc) -> do
          -- Perform the replacement
          suspendAndResume $ do
            performReplace searchPattern replaceText mwc
            let filepath = _mwcFilePath mwc
            -- Close dialog and trigger re-search by setting resultAction to NewSearch
            -- Track the modified file
            return $ st & replaceFocused .~ False
                        & replaceEdit .~ E.editor ReplaceEditor (Just 1) ""
                        & resultAction .~ Just NewSearch
                        & modifiedFiles %~ (filepath :)
          -- Exit the TUI to trigger re-search
          halt
    V.EvKey (V.KChar 'a') [V.MCtrl] -> do
      st <- get
      let replaceText = T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. replaceEdit)
          searchPattern = T.unpack $ T.strip $ T.unlines $ E.getEditContents (st ^. patternEdit)
          allMatches = Vec.toList $ listElements (st ^. matchList)
          affectedFiles = map _mwcFilePath allMatches
      -- Perform the replacement
      suspendAndResume $ do
        performReplaceAll searchPattern replaceText allMatches
        -- Close dialog and trigger re-search by setting resultAction to NewSearch
        -- Track all modified files
        return $ st & replaceFocused .~ False
                    & replaceEdit .~ E.editor ReplaceEditor (Just 1) ""
                    & resultAction .~ Just NewSearch
                    & modifiedFiles %~ (++ affectedFiles)
      -- Exit the TUI to trigger re-search
      halt
    _ -> zoom replaceEdit $ E.handleEditorEvent (VtyEvent e)

copyToClipboard :: MatchWithContext -> IO ()
copyToClipboard mwc = do
    let match    = _mwcMatch mwc
        filepath = _mwcFilePath mwc
        lineNum  = matchLine match
        text     = filepath ++ ":" ++ show lineNum
    _ <- readProcess "pbcopy" [] text
    pure ()

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
      case editor of
        "vim" -> let command = plusLineSyntax lineNum filepath editor
                 in callCommand command
        "emacs" -> let command = plusLineSyntax lineNum filepath editor
                   in callCommand command
        "nano"-> let command = plusLineSyntax lineNum filepath editor
                   in callCommand command
        _ -> let command = suffixLineSyntax lineNum filepath editor
             in callCommand command

plusLineSyntax:: Int -> String -> String -> String
plusLineSyntax lineNum filepath editor = editor ++ " +" ++ show lineNum ++ " " ++ show filepath

suffixLineSyntax:: Int -> String -> String -> String
suffixLineSyntax lineNum filepath editor = editor ++  " " ++ show filepath ++ ":" ++ show lineNum

-- | Safe regex match that handles invalid patterns
safeRegexMatch :: String -> String -> Maybe (String, String, String, [String])
safeRegexMatch pattern text = unsafePerformIO $ do
  result <- try (evaluate (text =~ pattern :: (String, String, String, [String]))) :: IO (Either SomeException (String, String, String, [String]))
  case result of
    Left _ -> return Nothing  -- Invalid regex, return Nothing
    Right match@(_, matched, _, _) ->
      if null matched
        then return Nothing  -- No match
        else return (Just match)

-- | Replace capture group placeholders like \1, \2 with actual captured text
substituteCaptures :: String -> String -> [String] -> String
substituteCaptures replacement fullMatch captures = go replacement
  where
    go [] = []
    go ('\\':d:rest) | d >= '0' && d <= '9' =
      let idx = read [d] :: Int
          capture = if idx == 0
                      then fullMatch
                      else if idx <= length captures
                             then captures !! (idx - 1)
                             else "\\" ++ [d]  -- Keep literal if index out of range
       in capture ++ go rest
    go (c:rest) = c : go rest

-- | Replace the search pattern on a single match
performReplace :: String -> String -> MatchWithContext -> IO ()
performReplace searchPattern replaceText mwc = do
  let filepath = _mwcFilePath mwc
      match = _mwcMatch mwc
      lineNum = matchLine match

  -- Read file
  content <- System.IO.readFile filepath
  let fileLines = lines content

  -- Replace on the matched line only
  if lineNum > 0 && lineNum <= length fileLines
    then do
      let targetLine = fileLines !! (lineNum - 1)
          -- Use regex substitution with capture group support
          replacedLine = case safeRegexMatch searchPattern targetLine of
            Nothing -> targetLine  -- Invalid regex or no match
            Just (before, matched, after, captures) ->
              let substituted = substituteCaptures replaceText matched captures
               in before ++ substituted ++ after
          modifiedLines = take (lineNum - 1) fileLines ++ [replacedLine] ++ drop lineNum fileLines

      -- Write back
      System.IO.writeFile filepath (unlines modifiedLines)
    else
      putStrLn $ "Error: Line " ++ show lineNum ++ " out of range in " ++ filepath

-- | Replace all occurrences across all matches
performReplaceAll :: String -> String -> [MatchWithContext] -> IO ()
performReplaceAll searchPattern replaceText matches = do
  -- Group matches by file
  let groupedByFile = foldr groupByFile [] matches

  -- For each file, perform all replacements
  mapM_ (replaceInFile searchPattern replaceText) groupedByFile
  where
    groupByFile :: MatchWithContext -> [(FilePath, [MatchWithContext])] -> [(FilePath, [MatchWithContext])]
    groupByFile mwc [] = [(_mwcFilePath mwc, [mwc])]
    groupByFile mwc ((fp, mwcs):rest)
      | _mwcFilePath mwc == fp = (fp, mwc:mwcs) : rest
      | otherwise = (fp, mwcs) : groupByFile mwc rest

    replaceInFile :: String -> String -> (FilePath, [MatchWithContext]) -> IO ()
    replaceInFile pattern replacement (filepath, mwcs) = do
      content <- System.IO.readFile filepath
      let fileLines = lines content
          -- Get all line numbers to replace (sorted in descending order to preserve line numbers)
          lineNums = map (matchLine . _mwcMatch) mwcs
          -- Perform replacements
          modifiedLines = replaceLines pattern replacement fileLines lineNums 1

      System.IO.writeFile filepath (unlines modifiedLines)

    replaceLines :: String -> String -> [String] -> [Int] -> Int -> [String]
    replaceLines _ _ [] _ _ = []
    replaceLines pattern replacement (line:rest) targets currentLine =
      let newLine = if currentLine `elem` targets
                      then case safeRegexMatch pattern line of
                        Nothing -> line  -- Invalid regex or no match
                        Just (before, matched, after, captures) ->
                          let substituted = substituteCaptures replacement matched captures
                           in before ++ substituted ++ after
                      else line
       in newLine : replaceLines pattern replacement rest targets (currentLine + 1)

toggleSearchType :: SearchType -> [SearchType] -> [SearchType]
toggleSearchType t ts
  | t `elem` ts = filter (/= t) ts
  | otherwise = t : ts

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
          spSearchTypes = types',
          spModifiedFiles = finalState ^. modifiedFiles
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
  let initState = (initialState previousMatches pattern path exts types) & loadingState .~ SearchingCache
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
          spSearchTypes = types',
          spModifiedFiles = finalState ^. modifiedFiles
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
          spSearchTypes = types,
          spModifiedFiles = []  -- No files modified yet in interactive mode
        }
    _ -> return Nothing  -- User quit without searching
