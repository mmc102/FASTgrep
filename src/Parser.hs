module Parser
  ( Lang (..),
    parseFile,
    ParseTree,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.Ptr (Ptr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)
import TreeSitter.Go qualified as Go
import TreeSitter.Haskell qualified as Haskell
import TreeSitter.Language qualified as TSLang
import TreeSitter.Parser
import TreeSitter.Python qualified as Python
import TreeSitter.Ruby qualified as Ruby
import TreeSitter.TSX qualified as TSX
import TreeSitter.Tree
import TreeSitter.TypeScript qualified as TypeScript

data Lang
  = Python
  | Go
  | Haskell
  | TypeScript
  | TSX
  | Ruby
  deriving (Show, Eq)

type ParseTree = Ptr Tree

parseFile :: Lang -> ByteString -> Either String (Ptr Tree)
parseFile lang content = unsafePerformIO $ do
  parser <- ts_parser_new
  _ <- ts_parser_set_language parser (languageGrammar lang)
  treePtr <- BS.useAsCStringLen content $ \(str, len) ->
    ts_parser_parse_string parser nullPtr str (fromIntegral len)
  return $ Right treePtr

languageGrammar :: Lang -> Ptr TSLang.Language
languageGrammar Python = Python.tree_sitter_python
languageGrammar Go = Go.tree_sitter_go
languageGrammar Haskell = Haskell.tree_sitter_haskell
languageGrammar TypeScript = TypeScript.tree_sitter_typescript
languageGrammar TSX = TSX.tree_sitter_tsx
languageGrammar Ruby = Ruby.tree_sitter_ruby
