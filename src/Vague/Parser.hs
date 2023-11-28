module Vague.Parser where

import Vague.FastString (FastString)
import Vague.Lexer (LStream (..), Token (..))
import qualified Vague.Lexer as Lexer
import Vague.Located
import Vague.Parser.Structure ()
import qualified Vague.Parser.Syntax as Syntax
import Prelude hiding (span)

type Parser a b = (Span -> a -> Lexer.LStream -> b) -> Lexer.LStream -> b

--------------------------------------------------------------------------------
type Result = ()

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

type Declaration = ()

parseDeclaration :: Parser Declaration Result
parseDeclaration k = withToken $ \span tok ->
  withToken $ \span' tok' -> undefined
