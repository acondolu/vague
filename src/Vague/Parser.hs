module Vague.Parser
  ( parse,
    Error,
  )
where

import Vague.FastString (FastString)
import Vague.Lexer (LStream (..), Token (..))
import qualified Vague.Lexer as Lexer
import Vague.Located
import Vague.Parser.Error (Error)
import qualified Vague.Parser.Structure as Structure
import Vague.Parser.Syntax
import Prelude hiding (span)

type Result = ()

-- parse :: Lexer.LStream -> Either Error Program
parse :: Lexer.LStream -> Either Error Structure.Tree
parse stream = do
  struct <- Structure.toStructure stream
  pure $ Structure.toTree struct

--------------------------------------------------------------------------------
type Parser a b = (Span -> a -> Lexer.LStream -> b) -> Lexer.LStream -> b

withToken :: Parser Lexer.Token Result
withToken k (LToken span tok stream) = k span tok stream
withToken _ (LError err) = error $ show err
withToken _ LEnd = error "don't know what to do with it"

rewind :: Span -> Token -> LStream -> LStream
rewind = LToken

type Pattern = ()

parsePattern :: Parser Pattern Result
parsePattern k = withToken $ \span -> \case
  Qualid xs x -> undefined
  _ -> undefined

parseDeclaration :: Parser Declaration Result
parseDeclaration k = withToken $ \span tok ->
  withToken $ \span' tok' -> undefined
