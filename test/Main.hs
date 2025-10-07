{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString qualified as BS
import Matcher
import Parser
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "fastgrep tests"
    [ pythonTests,
      tsxTests,
      rubyTests
    ]

pythonTests :: TestTree
pythonTests =
  testGroup
    "Python tests"
    [ testCase "Find function definition 'cat'" $ do
        content <- BS.readFile ("tests" </> "tests.py")
        case parseFile Python content of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tree -> do
            let matches = findMatches [FunctionDef] "cat" tree content
            length matches @?= 1
            matchLine (head matches) @?= 1,
      testCase "Find literal 'cat' in string" $ do
        content <- BS.readFile ("tests" </> "tests.py")
        case parseFile Python content of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tree -> do
            let matches = findMatches [Literal] "cat" tree content
            length matches @?= 1
            matchLine (head matches) @?= 5,
      testCase "Find identifier 'cat'" $ do
        content <- BS.readFile ("tests" </> "tests.py")
        case parseFile Python content of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tree -> do
            let matches = findMatches [Identifier] "cat" tree content
            length matches @?= 3 -- line 4, 8, 12
    ]

tsxTests :: TestTree
tsxTests =
  testGroup
    "TSX tests"
    [ testCase "Find function definition 'cat'" $ do
        content <- BS.readFile ("tests" </> "tests.tsx")
        case parseFile TSX content of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tree -> do
            let matches = findMatches [FunctionDef] "cat" tree content
            length matches @?= 1
            matchLine (head matches) @?= 1,
      testCase "Find literal 'cat' in string" $ do
        content <- BS.readFile ("tests" </> "tests.tsx")
        case parseFile TSX content of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tree -> do
            let matches = findMatches [Literal] "cat" tree content
            -- Should find "cat" but not "catacomb"
            let catMatches = filter (\m -> matchText m == "\"cat\"") matches
            length catMatches @?= 1
            matchLine (head catMatches) @?= 2,
      testCase "Find JSX text 'cat'" $ do
        content <- BS.readFile ("tests" </> "tests.tsx")
        case parseFile TSX content of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tree -> do
            let matches = findMatches [JsxText] "cat" tree content
            length matches @?= 1
            matchLine (head matches) @?= 4
    ]

rubyTests :: TestTree
rubyTests =
  testGroup
    "Ruby tests"
    [ testCase "Find function definition 'cat'" $ do
        content <- BS.readFile ("tests" </> "tests.rb")
        case parseFile Ruby content of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tree -> do
            let matches = findMatches [FunctionDef] "cat" tree content
            length matches @?= 1
            matchLine (head matches) @?= 2,
      testCase "Find function call 'cat'" $ do
        content <- BS.readFile ("tests" </> "tests.rb")
        case parseFile Ruby content of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tree -> do
            let matches = findMatches [FunctionCall] "cat" tree content
            length matches @?= 1
            matchLine (head matches) @?= 12,
      testCase "Find literal 'cat'" $ do
        content <- BS.readFile ("tests" </> "tests.rb")
        case parseFile Ruby content of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tree -> do
            let matches = findMatches [Literal] "cat" tree content
            length matches @?= 2 -- lines 7 and 8
    ]
