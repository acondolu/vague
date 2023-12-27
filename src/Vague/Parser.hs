{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Vague.Parser
  ( parse,
    PsError,
  )
where

import Control.Applicative (Alternative (..))
import qualified Data.Map as Map
import Vague.FastString (FastString, fsShow)
import Vague.Lexer (Token (..))
import qualified Vague.Lexer as Lexer
import Vague.Located
import Vague.Parser.Error (PsError (..))
import Vague.Parser.Syntax
import Vague.Parser.Units
import Prelude hiding (span)

-- | Parse a token stream into a 'Program'.
parse :: Lexer.LxStream -> Either PsError Program
parse ls = units ls >>= parseProgram

parseProgram :: Units -> Either PsError Program
parseProgram [] = Right $ Program [] []
parseProgram us = do
  (us', imports) <- parseImports us
  Program imports <$> parseBlock us'

parseImports :: Units -> Either PsError (Units, [Import])
parseImports = go []
  where
    go is [] = pure ([], reverse is)
    go is (PToken span (Keyword "import") : us) = do
      (us', i) <- pModuleName us
      go (ImQualified i : is) us'
    go is us = pure (us, reverse is)

pModuleName :: Units -> Either PsError (Units, ModuleName)
pModuleName = undefined

parseBlock :: Units -> Either PsError [Statement]
parseBlock [] = Right []
parseBlock us = do
  (us', stmt) <- parseStmt us
  stmts <- parseBlock us'
  pure $ stmt : stmts

parseStmt :: Units -> Either PsError (Units, Statement)
parseStmt us = do
  case takeUntil us of
    Nothing -> do
      e <- parseExpr us
      pure ([], Statement e)
    Just ([], Located _ "type", us') -> parseTypeBinding us'
    Just (first, Located _ "=", us') -> parseBinding first us'
    Just (first, Located _ ":", us') -> parseTypeDeclaration first us'
    Just (first, Located _ ";", us') -> do
      e <- parseExpr first
      pure (us', Statement e)
    Just (_, Located _ x, _) -> error $ "parseStmt: unknown case: " <> fsShow x <> " /= " <> fsShow "=" -- fixme error

parseTypeBinding :: Units -> Either PsError (Units, Statement)
parseTypeBinding = undefined

parseBinding :: Units -> Units -> Either PsError (Units, Statement)
parseBinding pat us = do
  -- TODO: parse also assignment to mutable record attributes,
  -- like expr.name1.name2.name3 = expr'
  pat' <- parsePattern pat
  case takeUntil us of
    Nothing -> do
      expr <- parseExpr us
      pure ([], Binding pat' expr)
    Just (first, Located _ ";", us') -> do
      expr <- parseExpr first
      pure (us', Binding pat' expr)
    Just (_, Located (Span loc _) sym, _) ->
      Left $ UnexpectedToken loc (Keyword sym)

parsePattern :: Units -> Either PsError Pattern
parsePattern = traverse go
  where
    go (PToken _ (Qualid "" name)) = pure name
    go (PToken (Span loc _) tok) = Left $ UnexpectedToken loc tok
    go (PBrack (Span loc _) ty _) = Left $ UnexpectedToken loc (openOf ty)

parseTypeDeclaration :: Units -> Units -> Either PsError (Units, Statement)
parseTypeDeclaration = undefined

parseRecord :: Units -> Either PsError [(Pattern, Expr)]
parseRecord [] = Right []
parseRecord us = do
  (us', row) <- parseRecordRow us
  rows <- parseRecord us'
  pure $ row : rows

parseRecordRow :: Units -> Either PsError (Units, (Pattern, Expr))
parseRecordRow us =
  case takeUntil us of
    Just (a, Located _ sym, us') -> do
      pat <- parsePattern a
      case sym of
        "=" -> case takeUntil us' of
          Nothing -> do
            expr <- parseExpr us'
            pure ([], (pat, expr))
          Just (b, Located _ ";", us'') -> do
            expr <- parseExpr b
            pure (us'', (pat, expr))
          Just (_, Located (Span loc _) sym', _) ->
            Left $ UnexpectedToken loc (Keyword sym')
        _ -> undefined
    Nothing -> do
      _ <- parsePattern us
      error "BUG: parseRecordRow" -- TODO: expcted '='

--------------------------------------------------------------------------------

exprOfPBrack :: BracketType -> Units -> Either PsError Expr
exprOfPBrack Round us = parseExpr us
exprOfPBrack Scope us = Block <$> parseBlock us
exprOfPBrack Curly us = RecordE <$> parseRecord us
exprOfPBrack Square _ = error "exprOfPBrack: Square TODO"

-- | This is for simple values, like identifiers, numbers, literals.
exprOfToken :: Span -> Token -> Either PsError Expr
exprOfToken span (Qualid qs n) = pure $ IdE qs n
exprOfToken span (Decimal n) = pure $ NumberE n
exprOfToken span (Literal s) = pure $ LitE s
exprOfToken (Span loc _) tok = Left $ UnexpectedToken loc tok

mkApp :: Expr -> [Expr] -> Expr
mkApp h [] = h
mkApp h tl = AppE h tl

-- | Parse all units in input as an expression.
-- Uses the "shunting yard" algorithm to handle infix operators.
-- See https://en.wikipedia.org/wiki/Shunting_yard_algorithm.
parseExpr :: Units -> Either PsError Expr
parseExpr = go [] [] []
  where
    go :: [Expr] -> [FastString] -> [Expr] -> Units -> Either PsError Expr
    go operands operators cur (PBrack _ btype ins : us) = do
      e' <- exprOfPBrack btype ins
      go operands operators (e' : cur) us
    go operands operators cur (PToken s@(Span loc _) tok : us) = case tok of
      Symbol name _loose -> case reverse cur of
        [] -> Left $ UnexpectedToken loc tok
        h : tl -> do
          (operands', operators') <- popUntil (fixities name) operators (mkApp h tl : operands)
          go operands' (name : operators') [] us
      _ -> do
        e <- exprOfToken s tok
        go operands operators (e : cur) us
    -- Before entering the final phase, collect the current operand
    -- and push it onto the output
    go operands operators cur@(_ : _) [] = case reverse cur of
      h : tl -> go (mkApp h tl : operands) operators [] []
      [] -> error "impossible"
    -- Final phase: pop all operators onto the output
    go operands (name : operators) [] [] = do
      operands' <- mkOp name (fixities name) operands
      go operands' operators [] []
    -- Return result
    go [operand] [] [] [] = pure operand
    -- Error when there are multiple results
    go operands [] [] [] = error $ "BUG: too many operands: " <> show operands

-- Pop operators from the stack according to precedences.
popUntil :: Fixity -> [FastString] -> [Expr] -> Either PsError ([Expr], [FastString])
popUntil _ [] operands = pure (operands, [])
popUntil fx (name : operators') operands
  | fx << fixities name = do
      operands' <- mkOp name fx operands
      popUntil fx operators' operands'
popUntil _ operators operands =
  pure (operands, operators)

mkOp :: FastString -> Fixity -> [Expr] -> Either PsError [Expr]
mkOp name (Fixity (Binary _) _) (e2 : e1 : es) =
  -- fixme! spans are lost ...
  pure $ AppE (IdE "" name) [e1, e2] : es
mkOp name (Fixity Unary _) (e : es) =
  pure $ AppE (IdE "" name) [e] : es
mkOp _name _ [] = Left $ error "mkOp: no operands"
mkOp _name _ [_] = Left $ error "mkOp: not enough operands"

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
        [ ("*", Fixity (Binary ALeft) 7),
          ("+", Fixity (Binary ALeft) 6)
        ]

--------------------------------------------------------------------------------

data Result a = RError PsError | ROk Units a

instance Functor Result where
  fmap _ (RError err) = RError err
  fmap f (ROk us a) = ROk us (f a)

newtype Parser a = Parser (Units -> Result a)

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap f . p

instance Applicative Parser where
  pure x = Parser $ \us -> ROk us x
  Parser f <*> Parser a = Parser $ \us -> case f us of
    RError err -> RError err
    ROk us' f' -> f' <$> a us'

instance Monad Parser where
  Parser p >>= f = Parser $ \us -> case p us of
    RError err -> RError err
    ROk us' a -> case f a of
      Parser g -> g us'

instance Alternative Parser where
  empty = Parser $ \_ -> RError undefined
  Parser f <|> Parser g = Parser $ \us ->
    case f us of
      RError _ -> g us
      ROk us' a -> ROk us' a

runParser :: Parser a -> Units -> Either PsError a
runParser (Parser a) us = case a us of
  RError err -> Left err
  ROk [] a -> Right a
  ROk us _ -> error "TODO"
