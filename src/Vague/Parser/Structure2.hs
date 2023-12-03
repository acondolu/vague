module Vague.Parser.Structure2
  ( PUnit (..),
    BracketType (..),
    units,
  )
where

import Vague.Lexer (LStream (..), Token (..))
import qualified Vague.Lexer as Lexer
import Vague.Located
import qualified Vague.Parser.Error as Error
import Prelude hiding (Word, span, words)

data BracketType = Round | Square | Curly | Scope
  deriving (Show)

data PUnit
  = PBrack BracketType [PUnit]
  | PToken Span Token
  deriving (Show)

units :: Lexer.LStream -> Either Error.Error [PUnit]
units = \stream -> do
  (_, out) <- go [] stream
  pure out
  where
    go :: [PUnit] -> Lexer.LStream -> Either Error.Error (Lexer.LStream, [PUnit])
    go acc s@(LToken span tok stream)
      | tok `elem` [RRound, RCurly, RSquare, ScopeEnd]
          = pure (s, reverse acc)
      | otherwise =
          case matching tok of
            Nothing -> go (PToken span tok : acc) stream
            Just (ty, right) -> do
              (stream', out) <- go [] stream
              stream'' <- expect right stream'
              go (PBrack ty out : acc) stream''
    go _ (LError err) = Left $ Error.fromLexerError err
    go acc LEnd = pure (LEnd, reverse acc)

expect :: Token -> LStream -> Either Error.Error LStream
expect tok (LToken (Span loc _) tok' stream'')
  | tok == tok' = pure stream''
  | otherwise = Left $ Error.ExpectedToken tok loc tok'
expect tok ls = error $ show (tok, ls)
expect _ _ = undefined

matching :: Token -> Maybe (BracketType, Token)
matching LRound = Just (Round, RRound)
matching LSquare = Just (Square, RSquare)
matching LCurly = Just (Curly, RCurly)
matching ScopeBegin = Just (Scope, ScopeEnd)
matching _ = Nothing

