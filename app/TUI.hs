{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (runSearchResultsTUI, runInteractiveTUI, MatchWithContext (..), SearchParams (..), ResultsAction (..)) where

import Brick
import Brick.Focus qualified as F
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List
import Control.Monad (void)
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

-- | Resource names
data Name
  = MatchListName
  | PatternEditor
  | PathEditor
  | ExtensionEditor
  deriving (Eq, Ord, Show)

-- | Application state
data AppState = AppState
  { _matchList :: List Name MatchWithContext,
    _selectedIndex :: Int
  }

makeLenses ''AppState

-- | Application state with action
data AppStateWithAction = AppStateWithAction
  { _appState :: AppState,
    _resultAction :: Maybe ResultsAction
  }

makeLenses ''AppStateWithAction

-- | Initialize the app state
initialState :: [MatchWithContext] -> AppState
initialState matches =
  AppState
    { _matchList = list MatchListName (Vec.fromList matches) 1,
      _selectedIndex = 0
    }

-- | Draw the UI
drawUI :: AppStateWithAction -> [Widget Name]
drawUI st =
  [ vBox
      [ drawHeader,
        hBorder,
        drawMatchList (st ^. appState),
        hBorder,
        drawMatchDetail (st ^. appState),
        hBorder,
        drawHelp
      ]
  ]

drawHeader :: Widget Name
drawHeader =
  withAttr (attrName "header") $
    padLeftRight 1 $
      str "fastgrep - AST-aware grep (Interactive Mode)"

drawMatchList :: AppState -> Widget Name
drawMatchList st =
  let total = Vec.length (listElements (st ^. matchList))
      current = maybe 0 (+ 1) (listSelected (st ^. matchList))
   in vLimit 10 $
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

drawHelp :: Widget Name
drawHelp =
  padLeftRight 1 $
    str "↑/k: up | ↓/j: down | n/s: new search | q/ESC: quit"

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
handleEvent :: BrickEvent Name e -> EventM Name AppStateWithAction ()
handleEvent (VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> modify (\s -> s & resultAction .~ Just Quit) >> halt
    V.EvKey (V.KChar 'q') [] -> modify (\s -> s & resultAction .~ Just Quit) >> halt
    V.EvKey (V.KChar 'n') [] -> modify (\s -> s & resultAction .~ Just NewSearch) >> halt
    V.EvKey (V.KChar 's') [] -> modify (\s -> s & resultAction .~ Just NewSearch) >> halt
    V.EvKey V.KDown [] -> modify $ \st -> st & appState %~ (\s -> s {_matchList = listMoveDown (_matchList s)})
    V.EvKey (V.KChar 'j') [] -> modify $ \st -> st & appState %~ (\s -> s {_matchList = listMoveDown (_matchList s)})
    V.EvKey V.KUp [] -> modify $ \st -> st & appState %~ (\s -> s {_matchList = listMoveUp (_matchList s)})
    V.EvKey (V.KChar 'k') [] -> modify $ \st -> st & appState %~ (\s -> s {_matchList = listMoveUp (_matchList s)})
    _ -> return ()
handleEvent _ = return ()

-- | Attribute map
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (attrName "header", V.cyan `on` V.black `V.withStyle` V.bold),
      (attrName "selected", V.black `on` V.white),
      (attrName "detail-header", fg V.yellow `V.withStyle` V.bold),
      (attrName "line-num", fg V.brightBlack),
      (attrName "line-num-highlight", fg V.yellow `V.withStyle` V.bold),
      (attrName "context", fg V.white),
      (attrName "type-literal", fg V.yellow `V.withStyle` V.bold),
      (attrName "type-identifier", fg V.cyan `V.withStyle` V.bold),
      (attrName "type-function-call", fg V.magenta `V.withStyle` V.bold),
      (attrName "type-function-def", fg V.green `V.withStyle` V.bold),
      (attrName "type-jsx-text", fg V.blue `V.withStyle` V.bold)
    ]

-- | The app definition
app :: App AppStateWithAction e Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

-- | Run the TUI with search results, returns the action to take next
runSearchResultsTUI :: [MatchWithContext] -> IO ResultsAction
runSearchResultsTUI matches = do
  let buildVty = mkVty V.defaultConfig
      initialAppState = AppStateWithAction
        { _appState = initialState matches,
          _resultAction = Nothing
        }
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty Nothing app initialAppState
  return $ fromMaybe Quit (finalState ^. resultAction)

-- ============================================================================
-- Interactive TUI (for search configuration)
-- ============================================================================

-- | Interactive app state
data InteractiveState = InteractiveState
  { _patternEdit :: E.Editor Text Name,
    _pathEdit :: E.Editor Text Name,
    _extensionEdit :: E.Editor Text Name,
    _searchTypes' :: [SearchType],
    _focusRing :: F.FocusRing Name,
    _statusMessage :: Maybe String
  }

makeLenses ''InteractiveState

-- | Initialize interactive state
initialInteractiveState :: String -> [String] -> InteractiveState
initialInteractiveState defaultPath defaultExts =
  InteractiveState
    { _patternEdit = E.editor PatternEditor (Just 1) "",
      _pathEdit = E.editor PathEditor (Just 1) (T.pack defaultPath),
      _extensionEdit = E.editor ExtensionEditor (Just 1) (T.pack $ unwords defaultExts),
      _searchTypes' = [],
      _focusRing = F.focusRing [PatternEditor, PathEditor, ExtensionEditor],
      _statusMessage = Nothing
    }

-- | Draw interactive UI
drawInteractiveUI :: InteractiveState -> [Widget Name]
drawInteractiveUI st =
  [ vBox
      [ drawInteractiveHeader,
        hBorder,
        padAll 1 $ drawSearchForm st,
        hBorder,
        padAll 1 $ drawSearchTypes st,
        hBorder,
        padLeftRight 1 $ drawInteractiveStatus st,
        hBorder,
        drawInteractiveHelp
      ]
  ]

drawInteractiveHeader :: Widget Name
drawInteractiveHeader =
  withAttr (attrName "header") $
    padLeftRight 1 $
      str "fastgrep - Interactive Search Configuration"

drawSearchForm :: InteractiveState -> Widget Name
drawSearchForm st =
  let focused = F.focusGetCurrent (st ^. focusRing)
   in vBox
        [ hBox
            [ str "Pattern: ",
              vLimit 1 $
                F.withFocusRing
                  (st ^. focusRing)
                  (E.renderEditor (txt . T.unlines))
                  (st ^. patternEdit)
            ],
          str " ",
          hBox
            [ str "Path:    ",
              vLimit 1 $
                F.withFocusRing
                  (st ^. focusRing)
                  (E.renderEditor (txt . T.unlines))
                  (st ^. pathEdit)
            ],
          str " ",
          hBox
            [ str "Exts:    ",
              vLimit 1 $
                F.withFocusRing
                  (st ^. focusRing)
                  (E.renderEditor (txt . T.unlines))
                  (st ^. extensionEdit)
            ]
        ]

drawSearchTypes :: InteractiveState -> Widget Name
drawSearchTypes st =
  vBox
    [ str "Search Types (toggle with keys):",
      str " ",
      str $ "[" ++ (if Literal `elem` st ^. searchTypes' then "X" else " ") ++ "] (l) Literal",
      str $ "[" ++ (if Identifier `elem` st ^. searchTypes' then "X" else " ") ++ "] (i) Identifier",
      str $ "[" ++ (if FunctionCall `elem` st ^. searchTypes' then "X" else " ") ++ "] (f) Function Call",
      str $ "[" ++ (if FunctionDef `elem` st ^. searchTypes' then "X" else " ") ++ "] (d) Function Definition",
      str $ "[" ++ (if JsxText `elem` st ^. searchTypes' then "X" else " ") ++ "] (j) JSX Text"
    ]

drawInteractiveStatus :: InteractiveState -> Widget Name
drawInteractiveStatus st =
  case st ^. statusMessage of
    Nothing -> str ""
    Just msg -> withAttr (attrName "status") $ str msg

drawInteractiveHelp :: Widget Name
drawInteractiveHelp =
  padLeftRight 1 $
    str "Tab: next field | l/i/f/d/j: toggle search types | Enter: search | q/ESC: quit"

-- | Handle interactive events
handleInteractiveEvent :: BrickEvent Name e -> EventM Name InteractiveState ()
handleInteractiveEvent (VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> halt
    V.EvKey (V.KChar 'q') [] -> halt
    V.EvKey (V.KChar '\t') [] -> modify $ \st -> st & focusRing %~ F.focusNext
    V.EvKey V.KBackTab [] -> modify $ \st -> st & focusRing %~ F.focusPrev
    V.EvKey (V.KChar 'l') [] -> modify $ \st -> st & searchTypes' %~ toggleSearchType Literal
    V.EvKey (V.KChar 'i') [] -> modify $ \st -> st & searchTypes' %~ toggleSearchType Identifier
    V.EvKey (V.KChar 'f') [] -> modify $ \st -> st & searchTypes' %~ toggleSearchType FunctionCall
    V.EvKey (V.KChar 'd') [] -> modify $ \st -> st & searchTypes' %~ toggleSearchType FunctionDef
    V.EvKey (V.KChar 'j') [V.MCtrl] -> modify $ \st -> st & searchTypes' %~ toggleSearchType JsxText
    V.EvKey V.KEnter [] -> do
      st <- get
      let patternText = T.unpack . T.concat $ E.getEditContents (st ^. patternEdit)
      if null (filter (not . null . T.unpack) $ E.getEditContents (st ^. patternEdit))
        then modify $ \s -> s & statusMessage .~ Just "Error: Pattern cannot be empty"
        else halt -- Will return and trigger search in runInteractiveTUI
    _ -> do
      st <- get
      case F.focusGetCurrent (st ^. focusRing) of
        Just PatternEditor -> zoom patternEdit $ E.handleEditorEvent (VtyEvent e)
        Just PathEditor -> zoom pathEdit $ E.handleEditorEvent (VtyEvent e)
        Just ExtensionEditor -> zoom extensionEdit $ E.handleEditorEvent (VtyEvent e)
        _ -> return ()
handleInteractiveEvent _ = return ()

toggleSearchType :: SearchType -> [SearchType] -> [SearchType]
toggleSearchType t ts
  | t `elem` ts = filter (/= t) ts
  | otherwise = t : ts

-- | Interactive attribute map
interactiveMap :: AttrMap
interactiveMap =
  attrMap
    V.defAttr
    [ (attrName "header", V.cyan `on` V.black `V.withStyle` V.bold),
      (attrName "status", fg V.red `V.withStyle` V.bold),
      (E.editAttr, V.white `on` V.black),
      (E.editFocusedAttr, V.black `on` V.white)
    ]

-- | The interactive app definition
interactiveApp :: App InteractiveState e Name
interactiveApp =
  App
    { appDraw = drawInteractiveUI,
      appChooseCursor = F.focusRingCursor (^. focusRing),
      appHandleEvent = handleInteractiveEvent,
      appStartEvent = return (),
      appAttrMap = const interactiveMap
    }

-- | Run interactive TUI - returns search parameters or Nothing if user quit
runInteractiveTUI :: a -> IO (Maybe SearchParams)
runInteractiveTUI _opts = do
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty Nothing interactiveApp (initialInteractiveState "." [])

  -- Extract search parameters
  let patternText = T.unpack . T.concat $ E.getEditContents (finalState ^. patternEdit)
      pathText = T.unpack . T.concat $ E.getEditContents (finalState ^. pathEdit)
      extText = T.unpack . T.concat $ E.getEditContents (finalState ^. extensionEdit)
      types = finalState ^. searchTypes'
      exts = filter (not . null) $ words extText

  -- Only return params if pattern is not empty
  if null (filter (not . null) [patternText])
    then return Nothing  -- User quit without searching
    else return $ Just SearchParams
      { spPattern = patternText,
        spPath = if null pathText then "." else pathText,
        spExtensions = exts,
        spSearchTypes = types
      }
