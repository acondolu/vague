{-# LANGUAGE OverloadedStrings #-}

module Vague.Parser.Units
  ( PUnit (..),
    Units,
    BracketType (..),
    units,
    takeUntil,
    openOf,
  )
where

import Vague.FastString (FastString)
import Vague.Lexer (LStream (..), Token (..))
import qualified Vague.Lexer as Lexer
import Vague.Located
import qualified Vague.Parser.Error as Error
import Prelude hiding (Word, span, words)

type Units = [PUnit]

data PUnit
  = PBrack Span BracketType [PUnit]
  | PToken Span Token
  deriving (Show)

data BracketType = Round | Square | Curly | Scope
  deriving (Show)

units :: Lexer.LStream -> Either Error.Error [PUnit]
units = \stream -> do
  (stream', out) <- go [] stream
  case stream' of
    LEnd -> pure out
    _ -> Left $ Error.Bug "units: LStream not consumed"
  where
    go :: [PUnit] -> Lexer.LStream -> Either Error.Error (Lexer.LStream, [PUnit])
    go acc s@(LToken span@(Span loca _) tok stream)
      | tok `elem` [RRound, RCurly, RSquare, ScopeEnd] =
          pure (s, reverse acc)
      | otherwise =
          case matching tok of
            Nothing -> go (PToken span tok : acc) stream
            Just (ty, right) -> do
              (stream', out) <- go [] stream
              (stream'', locb) <- expect right stream'
              go (PBrack (Span loca locb) ty out : acc) stream''
    go _ (LError err) = Left $ Error.fromLexerError err
    go acc LEnd = pure (LEnd, reverse acc)

expect :: Token -> LStream -> Either Error.Error (LStream, Loc)
expect tok (LToken (Span loc _) tok' stream'')
  | tok == tok' = pure (stream'', loc)
  | otherwise = Left $ Error.ExpectedToken tok loc tok'
expect _ (LError err) = Left $ Error.fromLexerError err
expect _ LEnd = Left Error.UnexpectedEOF

matching :: Token -> Maybe (BracketType, Token)
matching LRound = Just (Round, RRound)
matching LSquare = Just (Square, RSquare)
matching LCurly = Just (Curly, RCurly)
matching ScopeBegin = Just (Scope, ScopeEnd)
matching _ = Nothing

openOf :: BracketType -> Token
openOf Round = LRound
openOf Square = LSquare
openOf Curly = LCurly
openOf Scope = ScopeBegin

takeUntil :: Units -> Maybe (Units, Located FastString, Units)
takeUntil = go []
  where
    go _ [] = Nothing
    go acc (u@PBrack {} : us) = do
      go (u : acc) us
    go acc (u@(PToken span tok) : us) = case tok of
      Keyword k -> Just (reverse acc, Located span k, us)
      Symbol ";" -> Just (reverse acc, Located span ";", us)
      Symbol "=" -> Just (reverse acc, Located span "=", us)
      Symbol ":" -> Just (reverse acc, Located span ":", us)
      _ -> go (u : acc) us
