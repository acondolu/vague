module Vague.Parser.Structure where

import Vague.FastString (FastString)
import Vague.Lexer (LStream (..), Token (..))
import qualified Vague.Lexer as Lexer
import Vague.Located
import qualified Vague.Parser.Error as Error
import Prelude hiding (Word, span, words)

type Structure = [Unit]

data Unit
  = Group [Word]
  | USymbol Span Token

data BracketType = Round | Square | Curly | Scope

data Word
  = WIdent Span [FastString] FastString
  | WBrack BracketType Structure

parseIntermediate :: Lexer.LStream -> Either Error.Error Structure
parseIntermediate = \stream -> do
  let (result, rest) = go [] stream
  case rest of
    LToken (Span loc _) token _ -> Left $ Error.UnexpectedToken loc token
    LError err -> Left $ Error.fromLexerError err
    LEnd -> Right result
  where
    go words (LToken span tok stream) = case tok of
      Qualid xs x -> go (WIdent span xs x : words) stream
      Symbol _
        | null words -> do
            let (group, stream') = go [] stream
            (USymbol span tok : group, stream')
        | otherwise -> do
            let (group, stream') = go [] stream
            (USymbol span tok : Group (reverse words) : group, stream')
      LRound -> do
        let (group, stream') = go [] stream
        case stream' of
          LToken _ RRound stream'' -> go (WBrack Round group : words) stream''
          LToken (Span loc _) tok' _ -> do
            let _ = Error.ExpectedToken RRound loc tok' -- TODO
            error $ "expected rparens, found " <> show tok' <> " at " <> show loc
          LError _ -> ([], stream')
          LEnd -> ([], stream')
      _ -> undefined
    go words LEnd
      | null words = ([], LEnd)
      | otherwise = (Group (reverse words) : [], LEnd)
    go _ err@(LError _) = ([], err)
