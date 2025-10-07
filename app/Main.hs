{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes)
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

data Options = Options
  { searchPattern :: String,
    searchTypes :: [SearchType],
    targetPath :: FilePath,
    fileExtensions :: [String]
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
    <$> strArgument (metavar "PATTERN" <> help "The pattern to search for")
    <*> searchTypeParser
    <*> strOption (long "path" <> short 'p' <> value "." <> metavar "PATH" <> help "Path to search (file or directory)")
    <*> many (strOption (long "ext" <> short 'e' <> metavar "EXT" <> help "File extension to search (e.g., .py, .tsx). Can be specified multiple times."))

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
  isFile <- doesFileExist (targetPath opts)
  isDir <- doesDirectoryExist (targetPath opts)

  if isFile
    then searchFile opts (targetPath opts)
    else
      if isDir
        then searchDirectory opts (targetPath opts)
        else putStrLn $ "Error: " ++ targetPath opts ++ " is not a valid file or directory"

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
    ".vscode"
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
   in any (`matchPattern` name) patterns

matchPattern :: String -> String -> Bool
matchPattern pattern name
  | "/" `isSuffixOf` pattern = pattern `isPrefixOf` (name ++ "/")
  | "*" `isPrefixOf` pattern = drop 1 pattern `isSuffixOf` name
  | otherwise = pattern == name

searchFile :: Options -> FilePath -> IO ()
searchFile opts file = do
  let ext = takeExtension file
      shouldSearchFile = null (fileExtensions opts) || ext `elem` fileExtensions opts

  when shouldSearchFile $ do
    case languageForExtension ext of
      Nothing -> return ()
      Just lang -> do
        content <- BS.readFile file
        fileLines <- lines <$> readFile file
        case parseFile lang content of
          Left _err -> return ()
          Right tree -> do
            let matches = findMatches (searchTypes opts) (searchPattern opts) tree content
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
