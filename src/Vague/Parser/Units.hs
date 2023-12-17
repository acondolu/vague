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
import Vague.Lexer (LxStream (..), Token (..))
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

-- | Parenthesize the token stream.
units :: Lexer.LxStream -> Either Error.PsError [PUnit]
units = \stream -> do
  (stream', out) <- go [] stream
  case stream' of
    LEnd -> pure out
    _ -> Left $ Error.Bug "units: LxStream not consumed"
  where
    go :: [PUnit] -> Lexer.LxStream -> Either Error.PsError (Lexer.LxStream, [PUnit])
    go acc s@(LToken span@(Span loca _) tok stream)
      | tok `elem` [RRound, RCurly, RSquare, ScopeEnd] =
          pure (s, reverse acc)
      | otherwise =
          case closeOf tok of
            Nothing -> go (PToken span tok : acc) stream
            Just (ty, right) -> do
              (stream', out) <- go [] stream
              (stream'', locb) <- expect right stream'
              go (PBrack (Span loca locb) ty out : acc) stream''
    go _ (LError err) = Left $ Error.fromLxError err
    go acc LEnd = pure (LEnd, reverse acc)

-- | Expect the given token to be the first in the stream.
-- Return a parsing error instead.
expect :: Token -> LxStream -> Either Error.PsError (LxStream, Loc)
expect tok (LToken (Span loc _) tok' stream'')
  | tok == tok' = pure (stream'', loc)
  | otherwise = Left $ Error.ExpectedToken tok loc tok'
expect _ (LError err) = Left $ Error.fromLxError err
expect _ LEnd = Left Error.UnexpectedEOF

openOf :: BracketType -> Token
openOf Round = LRound
openOf Square = LSquare
openOf Curly = LCurly
openOf Scope = ScopeBegin

closeOf :: Token -> Maybe (BracketType, Token)
closeOf LRound = Just (Round, RRound)
closeOf LSquare = Just (Square, RSquare)
closeOf LCurly = Just (Curly, RCurly)
closeOf ScopeBegin = Just (Scope, ScopeEnd)
closeOf _ = Nothing

takeUntil :: Units -> Maybe (Units, Located FastString, Units)
takeUntil = go []
  where
    go _ [] = Nothing
    go acc (u@PBrack {} : us) = do
      go (u : acc) us
    go acc (u@(PToken span tok) : us) = case tok of
      Keyword k -> Just (reverse acc, Located span k, us)
      _ -> go (u : acc) us
