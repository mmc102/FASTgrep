{-# LANGUAGE OverloadedStrings #-}

module Matcher
  ( Match (..),
    findMatches,
    SearchType (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (isInfixOf)
import Data.Word (Word32)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.TDFA ((=~))
import TreeSitter.Cursor
import TreeSitter.Node (Node, TSNode, nodeChildCount, nodeEndByte, nodeStartByte, nodeStartPoint, nodeTSNode, nodeType, pointColumn, pointRow)
import TreeSitter.Tree

data SearchType
  = Literal
  | Identifier
  | FunctionCall
  | FunctionDef
  | JsxText
  deriving (Show, Eq)

data Match = Match
  { matchLine :: Int,
    matchColumn :: Int,
    matchText :: String,
    matchType :: SearchType
  }
  deriving (Show, Eq)

findMatches :: [SearchType] -> String -> Ptr Tree -> ByteString -> [Match]
findMatches searchTypes pattern treePtr sourceCode = unsafePerformIO $ do
  alloca $ \(rootPtr :: Ptr Node) -> do
    ts_tree_root_node_p treePtr rootPtr
    root <- peek rootPtr
    alloca $ \tsNodePtr -> do
      poke tsNodePtr (nodeTSNode root)
      withCursor tsNodePtr $ \cursor -> do
        traverseWithCursor searchTypes pattern cursor sourceCode

getNodeTypes :: SearchType -> [String]
getNodeTypes Literal = ["string"]
getNodeTypes Identifier = ["identifier"]
getNodeTypes FunctionCall = ["call", "call_expression"]
getNodeTypes FunctionDef = ["function_definition", "function_declaration", "method_definition"]
getNodeTypes JsxText = ["jsx_text"]

traverseWithCursor :: [SearchType] -> String -> Ptr Cursor -> ByteString -> IO [Match]
traverseWithCursor searchTypes pattern cursor sourceCode = do
  currentNode <- alloca $ \(ptr :: Ptr Node) -> do
    _ <- ts_tree_cursor_current_node_p cursor ptr
    peek ptr

  nodeTypeStr <- peekCString (nodeType currentNode)
  let typesToCheck =
        if null searchTypes
          then [Literal, Identifier, FunctionCall, FunctionDef, JsxText]
          else searchTypes
      checkType st =
        let targetTypes = getNodeTypes st
         in if any (`matchesType` nodeTypeStr) targetTypes
              then checkNodeContent pattern currentNode sourceCode st
              else []

  hasChild <- ts_tree_cursor_goto_first_child cursor
  if hasChild
    then do
      -- First get matches from children
      siblingMatches <- processSiblings searchTypes pattern cursor sourceCode []
      _ <- ts_tree_cursor_goto_parent cursor

      -- Only include current node's matches if no children matched
      let currentMatches =
            if null siblingMatches
              then concatMap checkType typesToCheck
              else []
      return (currentMatches ++ siblingMatches)
    else do
      -- Leaf node - check for matches
      let currentMatches = concatMap checkType typesToCheck
      return currentMatches

processSiblings :: [SearchType] -> String -> Ptr Cursor -> ByteString -> [Match] -> IO [Match]
processSiblings searchTypes pattern cursor sourceCode acc = do
  matches <- traverseWithCursor searchTypes pattern cursor sourceCode
  let newAcc = acc ++ matches
  hasNextSibling <- ts_tree_cursor_goto_next_sibling cursor
  if hasNextSibling
    then processSiblings searchTypes pattern cursor sourceCode newAcc
    else return newAcc

matchesType :: String -> String -> Bool
matchesType target nodeTypeStr =
  target `isInfixOf` nodeTypeStr

checkNodeContent :: String -> Node -> ByteString -> SearchType -> [Match]
checkNodeContent pattern node sourceCode searchType
  | null pattern = [] -- Empty pattern matches nothing
  | otherwise =
      let startPoint = nodeStartPoint node
          startByte = fromIntegral (nodeStartByte node)
          endByte = fromIntegral (nodeEndByte node)
          startLine = fromIntegral (pointRow startPoint) + 1
          startCol = fromIntegral (pointColumn startPoint) + 1
          nodeText = BS.take (endByte - startByte) $ BS.drop startByte sourceCode
          nodeTextStr = BS8.unpack nodeText
       in case searchType of
            -- For function defs and calls, only check the name portion
            FunctionDef -> checkFunctionName pattern node sourceCode searchType
            FunctionCall -> checkFunctionName pattern node sourceCode searchType
            -- For other types, check the entire node
            _ ->
              let matches = nodeTextStr =~ pattern :: Bool
               in [Match startLine startCol nodeTextStr searchType | matches]

-- | For functions, only check if the function name matches (not the entire body/args)
checkFunctionName :: String -> Node -> ByteString -> SearchType -> [Match]
checkFunctionName pattern node sourceCode searchType = unsafePerformIO $ do
  let tsNode = nodeTSNode node
      childCount = nodeChildCount node

  -- Find the name child node (usually the first or second child)
  -- For function_definition: child 0 is often 'def', child 1 is the name
  -- For call_expression: child 0 is the function name
  nameNode <- findNameNode tsNode childCount

  case nameNode of
    Nothing -> return [] -- No name node found, don't match
    Just name -> do
      let startPoint = nodeStartPoint name
          startByte = fromIntegral (nodeStartByte name)
          endByte = fromIntegral (nodeEndByte name)
          startLine = fromIntegral (pointRow startPoint) + 1
          startCol = fromIntegral (pointColumn startPoint) + 1
          nameText = BS.take (endByte - startByte) $ BS.drop startByte sourceCode
          nameTextStr = BS8.unpack nameText
          matches = nameTextStr =~ pattern :: Bool
          -- Use the full node text for display
          fullStartByte = fromIntegral (nodeStartByte node)
          fullEndByte = fromIntegral (nodeEndByte node)
          fullNodeText = BS.take (fullEndByte - fullStartByte) $ BS.drop fullStartByte sourceCode
          fullNodeTextStr = BS8.unpack fullNodeText
      return [Match startLine startCol fullNodeTextStr searchType | matches]

findNameNode :: TSNode -> Word32 -> IO (Maybe Node)
findNameNode tsNode childCount
  | childCount == 0 = return Nothing
  | otherwise = alloca $ \tsNodePtr -> do
      poke tsNodePtr tsNode
      withCursor tsNodePtr $ \cursor -> do
        hasChild <- ts_tree_cursor_goto_first_child cursor
        if hasChild
          then findNameInSiblings cursor
          else return Nothing
  where
    findNameInSiblings :: Ptr Cursor -> IO (Maybe Node)
    findNameInSiblings cursor = do
      currentNode <- alloca $ \(ptr :: Ptr Node) -> do
        _ <- ts_tree_cursor_current_node_p cursor ptr
        peek ptr
      nodeTypeStr <- peekCString (nodeType currentNode)

      if nodeTypeStr `elem` ["identifier", "name", "property_identifier"]
        then return (Just currentNode)
        else do
          hasNextSibling <- ts_tree_cursor_goto_next_sibling cursor
          if hasNextSibling
            then findNameInSiblings cursor
            else return Nothing
