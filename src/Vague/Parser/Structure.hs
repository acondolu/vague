{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Vague.Parser.Structure
  ( Structure,
    Unit (..),
    Word (..),
    BracketType (..),
    toStructure,
    toTree,
    Tree (..),
    Fixity (..),
    Arity (..),
    fixities,
    (<<),
  )
where

import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Vague.FastString (FastString)
import Vague.Lexer (LStream (..), Token (..))
import qualified Vague.Lexer as Lexer
import Vague.Located
import qualified Vague.Parser.Error as Error
import Prelude hiding (Word, span, words)

type Structure = [Unit]

data Unit
  = Group [Word Structure] -- operand
  | USymbol Span FastString -- operator
  | UKeyword Span FastString -- keyword
  deriving (Show)

data BracketType = Round | Square | Curly | Scope
  deriving (Show)

data Word a
  = WIdent Span [FastString] FastString
  | WLiter Span ByteString
  | WDecim Span Integer
  | WBrack BracketType a
  deriving (Show, Functor)


toStructure :: Lexer.LStream -> Either Error.Error Structure
toStructure = \stream -> do
  let (result, rest) = go [] stream
  case rest of
    LToken (Span loc _) token _ -> Left $ Error.UnexpectedToken loc token
    LError err -> Left $ Error.fromLexerError err
    LEnd -> Right result
  where
    go :: [Word Structure] -> LStream -> ([Unit], LStream)
    go words s@(LToken span tok stream) = case tok of
      Qualid xs x -> go (WIdent span xs x : words) stream
      Literal str -> go (WLiter span str : words) stream
      Decimal n -> go (WDecim span n : words) stream
      Symbol n
        | null words -> do
            let (us, stream') = go [] stream
            (USymbol span n : us, stream')
        | otherwise -> do
            let (us, stream') = go [] stream
            (Group (reverse words) : USymbol span n : us, stream')
      LRound -> do
        let (us, stream') = go [] stream
        case stream' of
          LToken _ RRound stream'' ->
            go (WBrack Round us : words) stream''
          LToken (Span loc _) tok' _ -> do
            let _ = Error.ExpectedToken RRound loc tok' -- TODO
            error $ "expected rparens, found " <> show tok' <> " at " <> show loc
          LError _ -> ([], stream')
          LEnd -> ([], stream')
      LCurly -> do
        let (us, stream') = go [] stream
        case stream' of
          LToken _ RCurly stream'' ->
            go (WBrack Curly us : words) stream''
          LToken (Span loc _) tok' _ -> do
            let _ = Error.ExpectedToken RCurly loc tok' -- TODO
            error $ "expected rcurly, found " <> show tok' <> " at " <> show loc
          LError _ -> ([], stream')
          LEnd -> ([], stream')
      ScopeBegin -> do
        let (us, stream') = go [] stream
        case stream' of
          LToken _ ScopeEnd stream'' ->
            go (WBrack Scope us : words) stream''
          LToken (Span loc _) tok' _ -> do
            let _ = Error.ExpectedToken ScopeEnd loc tok' -- TODO
            error $ "expected scope end, found " <> show tok' <> " at " <> show loc
          LError _ -> ([], stream')
          LEnd -> ([], stream')
      Keyword kw -> do
        let (us, stream') = go [] stream
        if null words
          then (UKeyword span kw : us, stream')
          else (Group (reverse words) : UKeyword span kw : us, stream')
      _ ->
        if null words
          then ([], s)
          else ([Group $ reverse words], s)
    go words LEnd
      | null words = ([], LEnd)
      | otherwise = ([Group $ reverse words], LEnd)
    go _ err@(LError _) = ([], err)

--------------------------------------------------------------------------------

data Associativity = ALeft | ANo | ARight
  deriving (Show, Eq)

data Arity = Unary | Binary Associativity
  deriving (Show)

data Fixity = Fixity Arity Int
  deriving (Show)

(<<) :: Fixity -> Fixity -> Bool
Fixity (Binary _) n << Fixity (Binary ass') n' =
  n < n' || (n == n' && ass' == ALeft)
Fixity (Binary _) n << Fixity Unary n' =
  n <= n'
Fixity Unary _ << _ = False

--------------------------------------------------------------------------------
-- Fixity Declarations
-- see https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-820004.4.2

fixities :: FastString -> Fixity
fixities op = case Map.lookup op m of
  Just x -> x
  Nothing -> Fixity (Binary ANo) 9 -- default fixity
  where
    m =
      Map.fromList
        [ ("=", Fixity (Binary ARight) $ -1),
          (";", Fixity (Binary ARight) $ -2)
        ]

--------------------------------------------------------------------------------

-- | Shunting yard algorithm.
-- See https://en.wikipedia.org/wiki/Shunting_yard_algorithm .
sh :: Structure -> Structure
sh = go [] []
  where
    go operands operators (operand@Group {} : ws) =
      go (operand : operands) operators ws
    go operands operators (operator@(USymbol _span name) : ws) = do
      let (operands', operators') = popUntil operands operators name
      go operands' (operator : operators') ws
    go operands operators (u@(UKeyword span fs):ws) = do
      undefined
    go operands operators [] =
      reverse operands <> operators

popUntil :: [Unit] -> [Unit] -> FastString -> ([Unit], [Unit])
popUntil = \operands operators op -> go operands operators (fixities op)
  where
    go :: [Unit] -> [Unit] -> Fixity -> ([Unit], [Unit])
    go operands [] _ = (operands, [])
    go operands operators@(op@(USymbol _ n) : xs) fx
      | fx << fixities n = go (op : operands) xs fx
      | otherwise = (operands, operators)
    go _ _ _ = error "BUG: operand in operator stack"

--------------------------------------------------------------------------------

data Tree
  = TBin Span FastString Tree Tree
  | TUna Span FastString Tree
  | Leaf [Word Tree]
  deriving (Show)

toTree :: Structure -> Tree
toTree = go [] . sh
  where
    go [] [] = Leaf []
    go [x] [] = x
    go units [] = error $ "toTree: impossible: " <> show units
    go units (Group xs : ops) =
      go (Leaf (map (fmap toTree) xs) : units) ops
    go (b : a : units) (USymbol span fs : ops) =
      go (TBin span fs a b : units) ops
    go units ops =
      error $ "toTree: " <> show (units, ops)
