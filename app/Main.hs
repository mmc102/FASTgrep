{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (filterM, foldM, forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Foreign.Ptr (Ptr)
import Matcher
import Options.Applicative
import Parser
import System.Console.ANSI
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitWith)
import System.FilePath (takeExtension, takeFileName)
import System.FilePath qualified as FP
import System.IO (hFlush, stdout)
import TUI (AppEvent (..), MatchWithContext (..), ResultsAction (..), SearchParams (..), runInteractiveTUI, runSearchResultsTUI, runSearchResultsTUIWithAsync, spExtensions, spModifiedFiles, spPath, spPattern, spSearchTypes)
import TreeSitter.Tree (Tree)

-- | Cached AST data for a file
data CachedAST = CachedAST
  { astTree :: Ptr Tree,
    astContent :: BS.ByteString,
    astLines :: [String],
    astLang :: Lang
  }

-- | AST cache mapping file paths to their parsed trees
type ASTCache = Map FilePath CachedAST

-- | Update cache for files that have been modified
-- Re-parses the files and updates the cache
updateCacheForModifiedFiles :: [FilePath] -> ASTCache -> Options -> IO ASTCache
updateCacheForModifiedFiles modifiedFiles cache opts = do
  foldM updateFile cache modifiedFiles
  where
    updateFile :: ASTCache -> FilePath -> IO ASTCache
    updateFile currentCache filepath = do
      maybeNewCached <- cacheFile opts filepath
      case maybeNewCached of
        Just newCached -> return $ Map.insert filepath newCached currentCache
        Nothing -> return currentCache -- Keep old cache if re-parsing fails

data Options = Options
  { searchPattern :: Maybe String,
    searchTypes :: [SearchType],
    targetPath :: FilePath,
    fileExtensions :: [String],
    tuiMode :: Bool
  }
  deriving (Show)

searchTypeParser :: Parser [SearchType]
searchTypeParser =
  catMaybes
    <$> sequenceA
      [ optional $ flag' Literal (long "literal" <> short 'l' <> help "Search for string literals"),
        optional $ flag' Identifier (long "identifier" <> short 'i' <> help "Search for identifiers"),
        optional $ flag' FunctionCall (long "function" <> short 'f' <> help "Search for function calls"),
        optional $ flag' FunctionDef (long "function-def" <> short 'd' <> help "Search for function definitions"),
        optional $ flag' JsxText (long "jsx-text" <> short 'j' <> help "Search for JSX text content")
      ]

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional (strArgument (metavar "PATTERN" <> help "The pattern to search for"))
    <*> searchTypeParser
    <*> strOption (long "path" <> short 'p' <> value "." <> metavar "PATH" <> help "Path to search (file or directory)")
    <*> many (strOption (long "ext" <> short 'e' <> metavar "EXT" <> help "File extension to search (e.g., .py, .tsx). Can be specified multiple times."))
    <*> switch (long "tui" <> short 't' <> help "Launch interactive TUI mode")

main :: IO ()
main = do
  let parserInfo =
        info
          (optionsParser <**> helper)
          ( fullDesc
              <> progDesc "Search for specific AST node types in source code"
              <> header "fastgrep - AST-aware grep for code"
          )

  opts <- execParserWithBannerOnHelp parserInfo
  runSearch opts

execParserWithBannerOnHelp :: ParserInfo a -> IO a
execParserWithBannerOnHelp pinfo = do
  args <- getArgs
  let result = execParserPure defaultPrefs pinfo args
  handleParseResult' args result
  where
    handleParseResult' :: [String] -> ParserResult a -> IO a
    handleParseResult' _ (Success a) = pure a
    handleParseResult' args (Failure failure) = do
      progn <- getProgName
      let (msg, exitCode) = renderFailure failure progn
      when ("--help" `elem` args || "-h" `elem` args) printBanner
      putStr msg
      exitWith exitCode
    handleParseResult' _ (CompletionInvoked compl) = do
      progn <- getProgName
      msg <- execCompletion compl progn
      printBanner
      putStr msg
      exitSuccess

printBanner :: IO ()
printBanner = do
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "░██████████                               ░██       ░██           ░██          "
  putStrLn "░██                                       ░██                     ░██          "
  putStrLn "░██        ░██░████  ░███████   ░██████   ░██    ░██░██░████████  ░██          "
  putStrLn "░█████████ ░███     ░██    ░██       ░██  ░██   ░██ ░██░██    ░██              "
  putStrLn "░██        ░██      ░█████████  ░███████  ░███████  ░██░██    ░██              "
  putStrLn "░██        ░██      ░██        ░██   ░██  ░██   ░██ ░██░██    ░██              "
  putStrLn "░██        ░██       ░███████   ░█████░██ ░██    ░██░██░██    ░██              "
  putStrLn "                                                                               "
  putStrLn "                                                                               "
  putStrLn "                                                                               "
  putStrLn "   ░███      ░██████   ░██████████                                             "
  putStrLn "  ░██░██    ░██   ░██      ░██                                                 "
  putStrLn " ░██  ░██  ░██             ░██        ░████████ ░██░████  ░███████  ░████████  "
  putStrLn "░█████████  ░████████      ░██       ░██    ░██ ░███     ░██    ░██ ░██    ░██ "
  putStrLn "░██    ░██         ░██     ░██       ░██    ░██ ░██      ░█████████ ░██    ░██ "
  putStrLn "░██    ░██  ░██   ░██      ░██       ░██   ░███ ░██      ░██        ░███   ░██ "
  putStrLn "░██    ░██   ░██████       ░██        ░█████░██ ░██       ░███████  ░██░█████  "
  putStrLn "                                            ░██                     ░██        "
  putStrLn "                                      ░███████                      ░██        "
  putStrLn "                                                                                "
  setSGR [Reset]
  putStrLn ""

runSearch :: Options -> IO ()
runSearch opts = do
  case searchPattern opts of
    Nothing -> runInteractiveSearchLoop opts -- Launch interactive TUI when no pattern
    Just pattern -> do
      isFile <- doesFileExist (targetPath opts)
      isDir <- doesDirectoryExist (targetPath opts)

      if tuiMode opts
        then do
          -- Collect all matches for TUI mode
          matchesWithContext <-
            if isFile
              then collectMatchesFromFile opts (targetPath opts)
              else
                if isDir
                  then collectMatchesFromDirectory opts (targetPath opts)
                  else do
                    putStrLn $ "Error: " ++ targetPath opts ++ " is not a valid file or directory"

                    return []
          (action, newParams) <- runSearchResultsTUI matchesWithContext pattern (targetPath opts) (fileExtensions opts) (searchTypes opts)
          case action of
            NewSearch -> runSearchWithParams newParams
            Quit -> return ()
        else do
          -- Stream output in CLI mode
          if isFile
            then searchFile opts (targetPath opts)
            else
              if isDir
                then searchDirectory opts (targetPath opts)
                else putStrLn $ "Error: " ++ targetPath opts ++ " is not a valid file or directory"

-- | Loop between interactive search form and results
runInteractiveSearchLoop :: Options -> IO ()
runInteractiveSearchLoop opts = do
  maybeParams <- runInteractiveTUI opts
  case maybeParams of
    Nothing -> return () -- User quit
    Just params -> runSearchWithParams params

-- | Run search with given parameters - builds cache on first run
runSearchWithParams :: SearchParams -> IO ()
runSearchWithParams params = runSearchWithCache params Nothing

-- | Run search with optional cache (builds cache if not provided)
runSearchWithCache :: SearchParams -> Maybe ASTCache -> IO ()
runSearchWithCache params maybeCache = runSearchWithCacheAndResults params maybeCache []

-- | Run search with cache and previous results
runSearchWithCacheAndResults :: SearchParams -> Maybe ASTCache -> [MatchWithContext] -> IO ()
runSearchWithCacheAndResults params maybeCache previousResults = do
  let searchOpts =
        Options
          { searchPattern = Just (spPattern params),
            searchTypes = spSearchTypes params,
            targetPath = spPath params,
            fileExtensions = spExtensions params,
            tuiMode = True
          }

  -- Create IORef to capture the built cache
  cacheRef <- newIORef maybeCache

  -- Create async search action that streams results as files are parsed
  let searchAction chan = do
        case maybeCache of
          Just cache -> do
            -- Have cache, use it
            let matchesWithContext = searchCachedASTs cache (spPattern params) (spSearchTypes params)
            writeBChan chan (SearchComplete matchesWithContext)
          Nothing -> do
            -- No cache, stream results file-by-file for faster perceived speed
            streamSearchResults searchOpts (spPath params) (spPattern params) (spSearchTypes params) chan
            writeIORef cacheRef Nothing -- Don't cache for now in streaming mode

  -- Run TUI with async search
  (action, newParams) <-
    runSearchResultsTUIWithAsync
      previousResults
      (spPattern params)
      (spPath params)
      (spExtensions params)
      (spSearchTypes params)
      searchAction

  -- Get the final cache state
  finalCache <- readIORef cacheRef

  case action of
    NewSearch -> do
      -- Check if we need to rebuild cache
      if spPath newParams /= spPath params || spExtensions newParams /= spExtensions params
        then runSearchWithCacheAndResults newParams Nothing []
        else do
          -- Update cache for modified files only
          updatedCache <- case finalCache of
            Nothing -> return Nothing
            Just cache ->
              if null (spModifiedFiles newParams)
                then return (Just cache)
                else do
                  let searchOpts =
                        Options
                          { searchPattern = Just (spPattern newParams),
                            searchTypes = spSearchTypes newParams,
                            targetPath = spPath newParams,
                            fileExtensions = spExtensions newParams,
                            tuiMode = True
                          }
                  newCache <- updateCacheForModifiedFiles (spModifiedFiles newParams) cache searchOpts
                  return (Just newCache)
          runSearchWithCacheAndResults newParams updatedCache []
    Quit -> return ()

-- | Stream search results file-by-file (faster perceived speed than building cache first)
streamSearchResults :: Options -> FilePath -> String -> [SearchType] -> BChan AppEvent -> IO ()
streamSearchResults opts path pattern searchTypes chan = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path

  allMatches <- newIORef []

  if isFile
    then streamSearchFile opts path pattern searchTypes allMatches chan
    else
      if isDir
        then streamSearchDirectory opts path pattern searchTypes allMatches chan
        else return ()

  -- Send final results
  matches <- readIORef allMatches
  writeBChan chan (SearchComplete matches)

streamSearchDirectory :: Options -> FilePath -> String -> [SearchType] -> IORef [MatchWithContext] -> BChan AppEvent -> IO ()
streamSearchDirectory opts dir pattern searchTypes matchesRef chan = do
  entries <- listDirectory dir
  gitignorePatterns <- readGitignore (dir FP.</> ".gitignore")

  let filteredEntries = filter (not . shouldIgnore gitignorePatterns) entries
      fullPaths = map (dir FP.</>) filteredEntries

  files <- filterM doesFileExist fullPaths
  dirs <- filterM doesDirectoryExist fullPaths

  -- Search files in parallel
  mapConcurrently_ (\file -> streamSearchFile opts file pattern searchTypes matchesRef chan) files

  -- Search subdirectories
  forM_ dirs $ \subdir -> streamSearchDirectory opts subdir pattern searchTypes matchesRef chan

streamSearchFile :: Options -> FilePath -> String -> [SearchType] -> IORef [MatchWithContext] -> BChan AppEvent -> IO ()
streamSearchFile opts file pattern searchTypes matchesRef chan = do
  let ext = takeExtension file
      shouldSearchFile = null (fileExtensions opts) || ext `elem` fileExtensions opts

  when shouldSearchFile $ do
    case languageForExtension ext of
      Nothing -> return ()
      Just lang -> do
        content <- BS.readFile file
        let fileLines = lines (BS8.unpack content)
        case parseFile lang content of
          Left _err -> return ()
          Right tree -> do
            let matches = findMatches searchTypes pattern tree content
                matchesWithContext = map (createMatchWithContext file fileLines) matches
            -- Add to accumulator
            atomicModifyIORef' matchesRef (\ms -> (ms ++ matchesWithContext, ()))
            -- Send incremental update
            currentMatches <- readIORef matchesRef
            writeBChan chan (SearchComplete currentMatches)

-- | Build AST cache with progress updates sent via BChan
buildASTCacheWithProgress :: Options -> FilePath -> BChan AppEvent -> IO ASTCache
buildASTCacheWithProgress opts path chan = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path

  if isFile
    then do
      cache <- cacheFileWithProgress opts path chan 1 1
      return $ maybe Map.empty (\c -> Map.singleton path c) cache
    else
      if isDir
        then cacheDirectoryWithProgress opts path chan
        else return Map.empty

-- | Count total files to parse
countFiles :: Options -> FilePath -> IO Int
countFiles opts path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path

  if isFile
    then return 1
    else
      if isDir
        then countFilesInDirectory opts path
        else return 0

-- | Count files in directory
countFilesInDirectory :: Options -> FilePath -> IO Int
countFilesInDirectory opts dir = do
  entries <- listDirectory dir
  gitignorePatterns <- readGitignore (dir FP.</> ".gitignore")

  let filteredEntries = filter (not . shouldIgnore gitignorePatterns) entries
      fullPaths = map (dir FP.</>) filteredEntries

  files <- filterM doesFileExist fullPaths
  dirs <- filterM doesDirectoryExist fullPaths

  let matchingFiles =
        filter
          ( \f ->
              let ext = takeExtension f
               in null (fileExtensions opts) || ext `elem` fileExtensions opts
          )
          files

  dirCounts <- mapM (countFilesInDirectory opts) dirs
  return $ length matchingFiles + sum dirCounts

-- | Cache directory with progress updates
cacheDirectoryWithProgress :: Options -> FilePath -> BChan AppEvent -> IO ASTCache
cacheDirectoryWithProgress opts dir chan = do
  -- Count total files first
  totalFiles <- countFilesInDirectory opts dir

  -- Cache with progress tracking
  cacheDirectoryWithProgressHelper opts dir chan 0 totalFiles

-- | Helper for caching directory with progress
cacheDirectoryWithProgressHelper :: Options -> FilePath -> BChan AppEvent -> Int -> Int -> IO ASTCache
cacheDirectoryWithProgressHelper opts dir chan currentCount totalFiles = do
  entries <- listDirectory dir
  gitignorePatterns <- readGitignore (dir FP.</> ".gitignore")

  let filteredEntries = filter (not . shouldIgnore gitignorePatterns) entries
      fullPaths = map (dir FP.</>) filteredEntries

  files <- filterM doesFileExist fullPaths
  dirs <- filterM doesDirectoryExist fullPaths

  -- Create progress counter
  countRef <- newIORef currentCount

  -- Cache files in parallel with progress updates
  fileCaches <-
    mapConcurrently
      ( \file -> do
          count <- atomicModifyIORef' countRef (\c -> (c + 1, c))
          maybeCache <- cacheFileWithProgress opts file chan count totalFiles
          return $ case maybeCache of
            Just cache -> Map.singleton file cache
            Nothing -> Map.empty
      )
      files

  -- Get final count after all files
  newCount <- readIORef countRef

  -- Cache subdirectories (still need sequential for correct count)
  (dirCaches, _finalCount) <-
    foldM
      ( \(caches, count) subdir -> do
          cache <- cacheDirectoryWithProgressHelper opts subdir chan count totalFiles
          -- Count how many files were in this directory
          subFiles <- countFilesInDirectory opts subdir
          return (cache : caches, count + subFiles)
      )
      ([], newCount)
      dirs

  return $ Map.unions (fileCaches ++ dirCaches)

-- | Cache a single file with progress update
cacheFileWithProgress :: Options -> FilePath -> BChan AppEvent -> Int -> Int -> IO (Maybe CachedAST)
cacheFileWithProgress opts file chan currentCount totalFiles = do
  -- Send progress update first
  writeBChan chan (UpdateProgress (currentCount + 1) totalFiles)

  let ext = takeExtension file
      shouldCache = null (fileExtensions opts) || ext `elem` fileExtensions opts

  if not shouldCache
    then return Nothing
    else case languageForExtension ext of
      Nothing -> return Nothing
      Just lang -> do
        content <- BS.readFile file
        evaluate (BS.length content)
        let fileLines = lines (BS8.unpack content)
        case parseFile lang content of
          Left _err -> return Nothing
          Right tree -> do
            return $
              Just
                CachedAST
                  { astTree = tree,
                    astContent = content,
                    astLines = fileLines,
                    astLang = lang
                  }

-- | Build AST cache for all files in the given path
buildASTCache :: Options -> FilePath -> IO ASTCache
buildASTCache opts path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path

  if isFile
    then do
      cache <- cacheFile opts path
      return $ maybe Map.empty (\c -> Map.singleton path c) cache
    else
      if isDir
        then cacheDirectory opts path
        else do
          putStrLn $ "Error: " ++ path ++ " is not a valid file or directory"
          return Map.empty

-- | Cache ASTs for all files in a directory
cacheDirectory :: Options -> FilePath -> IO ASTCache
cacheDirectory opts dir = do
  entries <- listDirectory dir
  gitignorePatterns <- readGitignore (dir FP.</> ".gitignore")

  let filteredEntries = filter (not . shouldIgnore gitignorePatterns) entries
      fullPaths = map (dir FP.</>) filteredEntries

  files <- filterM doesFileExist fullPaths
  dirs <- filterM doesDirectoryExist fullPaths

  -- Cache files in parallel for speed
  fileCaches <-
    mapConcurrently
      ( \file -> do
          maybeCache <- cacheFile opts file
          return $ case maybeCache of
            Just cache -> Map.singleton file cache
            Nothing -> Map.empty
      )
      files

  -- Cache directories recursively (also in parallel)
  dirCaches <- mapConcurrently (\subdir -> cacheDirectory opts subdir) dirs

  return $ Map.unions (fileCaches ++ dirCaches)

-- | Cache a single file's AST
cacheFile :: Options -> FilePath -> IO (Maybe CachedAST)
cacheFile opts file = do
  let ext = takeExtension file
      shouldCache = null (fileExtensions opts) || ext `elem` fileExtensions opts

  if not shouldCache
    then return Nothing
    else case languageForExtension ext of
      Nothing -> return Nothing
      Just lang -> do
        content <- BS.readFile file
        evaluate (BS.length content)
        let fileLines = lines (BS8.unpack content)
        case parseFile lang content of
          Left _err -> return Nothing
          Right tree -> do
            return $
              Just
                CachedAST
                  { astTree = tree,
                    astContent = content,
                    astLines = fileLines,
                    astLang = lang
                  }

-- | Search using cached ASTs
searchCachedASTs :: ASTCache -> String -> [SearchType] -> [MatchWithContext]
searchCachedASTs cache pattern types =
  concat $ map searchCachedFile $ Map.toList cache
  where
    searchCachedFile (filepath, cached) =
      let matches = findMatches types pattern (astTree cached) (astContent cached)
       in map (createMatchWithContext filepath (astLines cached)) matches

collectMatchesFromDirectory :: Options -> FilePath -> IO [MatchWithContext]
collectMatchesFromDirectory opts dir = do
  entries <- listDirectory dir
  gitignorePatterns <- readGitignore (dir FP.</> ".gitignore")

  let filteredEntries = filter (not . shouldIgnore gitignorePatterns) entries
      fullPaths = map (dir FP.</>) filteredEntries

  files <- filterM doesFileExist fullPaths
  dirs <- filterM doesDirectoryExist fullPaths

  -- Process files strictly to avoid accumulating file handles
  fileMatches <- fmap concat $ forM files $ \file -> do
    matches <- collectMatchesFromFile opts file
    evaluate (length matches) -- Force evaluation before moving to next file
    return matches

  -- Process directories strictly
  dirMatches <- fmap concat $ forM dirs $ \subdir -> do
    matches <- collectMatchesFromDirectory opts subdir
    evaluate (length matches) -- Force evaluation before moving to next directory
    return matches

  let allMatches = fileMatches ++ dirMatches
  evaluate (length allMatches) -- Force evaluation of combined results
  return allMatches

collectMatchesFromFile :: Options -> FilePath -> IO [MatchWithContext]
collectMatchesFromFile opts file = do
  let ext = takeExtension file
      shouldSearchFile = null (fileExtensions opts) || ext `elem` fileExtensions opts

  case searchPattern opts of
    Nothing -> return []
    Just pattern -> do
      if not shouldSearchFile
        then return []
        else case languageForExtension ext of
          Nothing -> return []
          Just lang -> do
            content <- BS.readFile file
            evaluate (BS.length content)
            let fileLines = lines (BS8.unpack content) -- Convert ByteString to lines instead of reading twice
            case parseFile lang content of
              Left _err -> return []
              Right tree -> do
                let matches = findMatches (searchTypes opts) pattern tree content
                    results = map (createMatchWithContext file fileLines) matches
                evaluate (length results) -- Force evaluation of results
                return results

createMatchWithContext :: FilePath -> [String] -> Match -> MatchWithContext
createMatchWithContext file fileLines match =
  let lineNum = matchLine match
      contextBefore = 4
      contextAfter = 4
      startLine = max 1 (lineNum - contextBefore)
      endLine = min (length fileLines) (lineNum + contextAfter)
      contextLines = zip [startLine .. endLine] (take (endLine - startLine + 1) $ drop (startLine - 1) fileLines)
   in MatchWithContext match file contextLines

searchDirectory :: Options -> FilePath -> IO ()
searchDirectory opts dir = do
  entries <- listDirectory dir

  gitignorePatterns <- readGitignore (dir FP.</> ".gitignore")

  let filteredEntries = filter (not . shouldIgnore gitignorePatterns) entries
      fullPaths = map (dir FP.</>) filteredEntries

  files <- filterM doesFileExist fullPaths
  dirs <- filterM doesDirectoryExist fullPaths

  forM_ files $ \file -> searchFile opts file

  forM_ dirs $ \subdir -> searchDirectory opts subdir

defaultIgnorePatterns :: [String]
defaultIgnorePatterns =
  [ ".git",
    "dist-newstyle",
    "dist",
    ".stack-work",
    "node_modules",
    ".cabal-sandbox",
    "target",
    "build",
    ".idea",
    ".vscode",
    ".next",
    ".nuxt",
    ".turbo",
    "__pycache__",
    ".pytest_cache",
    "venv",
    ".venv",
    "env",
    ".env"
  ]

readGitignore :: FilePath -> IO [String]
readGitignore path = do
  exists <- doesFileExist path
  if exists
    then do
      content <- readFile path
      let patterns =
            filter (not . null)
              . filter (not . isPrefixOf "#")
              . map (dropWhile (== ' '))
              . lines
              $ content
      return (defaultIgnorePatterns ++ patterns)
    else return defaultIgnorePatterns

shouldIgnore :: [String] -> FilePath -> Bool
shouldIgnore patterns path =
  let name = takeFileName path
   in any (\p -> matchPattern p name path) patterns

matchPattern :: String -> String -> FilePath -> Bool
matchPattern pattern name fullPath
  -- Match directory patterns like "frontend/.next"
  | '/' `elem` pattern = pattern `isInfixOf` fullPath || pattern `isSuffixOf` fullPath
  -- Match trailing slash patterns like "node_modules/"
  | "/" `isSuffixOf` pattern = (init pattern) == name
  -- Match glob patterns like "*.log"
  | "*" `isPrefixOf` pattern = drop 1 pattern `isSuffixOf` name
  -- Exact match
  | otherwise = pattern == name

searchFile :: Options -> FilePath -> IO ()
searchFile opts file = do
  let ext = takeExtension file
      shouldSearchFile = null (fileExtensions opts) || ext `elem` fileExtensions opts

  case searchPattern opts of
    Nothing -> return ()
    Just pattern -> do
      when shouldSearchFile $ do
        case languageForExtension ext of
          Nothing -> return ()
          Just lang -> do
            content <- BS.readFile file
            let fileLines = lines (BS8.unpack content) -- Convert ByteString to lines instead of reading twice
            case parseFile lang content of
              Left _err -> return ()
              Right tree -> do
                let matches = findMatches (searchTypes opts) pattern tree content
                forM_ matches $ \match -> do
                  printMatchCard opts file fileLines match

printMatchCard :: Options -> FilePath -> [String] -> Match -> IO ()
printMatchCard opts file fileLines match = do
  let lineNum = matchLine match
      contextBefore = 2
      contextAfter = 2
      startLine = max 1 (lineNum - contextBefore)
      endLine = min (length fileLines) (lineNum + contextAfter)
      contextLines = zip [startLine .. endLine] (take (endLine - startLine + 1) $ drop (startLine - 1) fileLines)
      (typeColor, typeLabel) = getMatchTypeInfo (matchType match)

  setSGR [SetColor Foreground Vivid typeColor, SetConsoleIntensity BoldIntensity]
  putStr "┌─ "
  putStr typeLabel
  setSGR [Reset]
  putStr " "
  setSGR [SetColor Foreground Dull White]
  putStr $ file ++ ":" ++ show lineNum ++ ":" ++ show (matchColumn match)
  setSGR [Reset]
  putStrLn ""

  forM_ contextLines $ \(num, line) -> do
    if num == lineNum
      then do
        setSGR [SetColor Foreground Vivid typeColor]
        putStr "│ "
        setSGR [SetColor Foreground Dull White]
        putStr $ show num ++ " │ "
        setSGR [Reset, SetConsoleIntensity BoldIntensity]
        putStrLn line
        setSGR [Reset]
      else do
        setSGR [SetColor Foreground Dull typeColor]
        putStr "│ "
        setSGR [SetColor Foreground Dull White]
        putStr $ show num ++ " │ "
        setSGR [SetColor Foreground Dull White]
        putStrLn line
        setSGR [Reset]

  setSGR [SetColor Foreground Vivid typeColor]
  putStr "└─ "
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr $ matchText match
  setSGR [Reset]
  putStrLn ""
  putStrLn ""
  hFlush stdout

getMatchTypeInfo :: SearchType -> (Color, String)
getMatchTypeInfo Literal = (Yellow, "LITERAL")
getMatchTypeInfo Identifier = (Cyan, "IDENTIFIER")
getMatchTypeInfo FunctionCall = (Magenta, "FUNCTION CALL")
getMatchTypeInfo FunctionDef = (Green, "FUNCTION DEF")
getMatchTypeInfo JsxText = (Blue, "JSX TEXT")

languageForExtension :: String -> Maybe Lang
languageForExtension ext = case ext of
  ".py" -> Just Python
  ".go" -> Just Go
  ".hs" -> Just Haskell
  ".ts" -> Just TypeScript
  ".tsx" -> Just TSX
  ".js" -> Just TypeScript
  ".jsx" -> Just TSX
  ".rb" -> Just Ruby
  _ -> Nothing
