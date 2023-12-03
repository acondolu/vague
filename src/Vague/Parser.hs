{-# LANGUAGE OverloadedStrings #-}

module Vague.Parser
  ( parse,
    Error,
  )
where

import Vague.FastString (FastString)
import Vague.Lexer (LStream (..), Token (..))
import qualified Vague.Lexer as Lexer
import Vague.Located
import Vague.Parser.Error (Error (..))
import qualified Vague.Parser.Structure as Structure
import Vague.Parser.Structure (Fixity (..), fixities, (<<), Arity (..))
import qualified Vague.Parser.Structure2 as Structure2
import Vague.Parser.Syntax
import Prelude hiding (span)
import Data.Text (pack)

type Result = ()

parse :: Lexer.LStream -> Either Error Program
parse ls = Structure2.units ls >>= parseProgram
-- parse :: Lexer.LStream -> Either Error Structure.Tree
-- parse stream = do
--   struct <- Structure.toStructure stream
--   pure $ Structure.toTree struct

type Units = [Structure2.PUnit]

parseProgram :: Units -> Either Error Program
parseProgram [] = Right $ Program []
parseProgram us = Program <$> parseBlock us

parseBlock :: Units -> Either Error [Statement]
parseBlock [] = Right []
parseBlock us = do
  (us', stmt) <- parseStmt us
  stmts <- parseBlock us'
  pure $ stmt : stmts

parseStmt :: Units -> Either Error (Units, Statement)
parseStmt us = do
  let (first, sym, us') = takeUntil us
  case (first, sym) of
    ([], "type") -> parseTypeBinding us'
    (_, "=") -> parseBinding first us'
    (_, ":") -> parseTypeDeclaration first us'
    (_, ";") -> do
      e <- parseExpr first
      pure (us', Statement e)
    (_, "") -> do
      e <- parseExpr first
      pure ([], Statement e)
    _ -> error "parseStmt: unknown case" -- fixme error

parseExpr :: Units -> Either Error Expr
parseExpr = sh

parseTypeBinding :: Units -> Either Error (Units, Statement)
parseTypeBinding = undefined

parseBinding :: Units -> Units -> Either Error (Units, Statement)
parseBinding pat us = do
  pat' <- parsePattern pat
  let (first, sym, us') = takeUntil us
  case (first, sym) of
    (_, ";") -> do
      expr <- parseExpr first
      pure (us', Binding pat' expr)
    (_, "") -> do
      expr <- parseExpr first
      pure (us', Binding pat' expr)
    (_, sym) -> error $ "parseBinding: unknown case: " <> show sym -- fixme error

parsePattern :: Units -> Either Error Pattern
parsePattern = traverse go
  where
    go (Structure2.PToken _ (Qualid [] name)) = pure name
    go (Structure2.PToken (Span loc _) tok) = Left $ UnexpectedToken loc tok
    go (Structure2.PBrack ty _) = Left $ Bug $ "unexpected bracket: " <> pack (show ty)

parseTypeDeclaration :: Units -> Units -> Either Error (Units, Statement)
parseTypeDeclaration = undefined

takeUntil :: Units -> (Units, FastString, Units)
takeUntil = go []
  where
    go acc [] = (reverse acc, "", [])
    go acc (u@Structure2.PBrack {}:us) = do
      go (u:acc) us
    go acc (u@(Structure2.PToken _ tok) : us) = case tok of
      Keyword k -> (reverse acc, k, us)
      Symbol ";" -> (reverse acc, ";", us)
      Symbol "=" -> (reverse acc, "=", us)
      Symbol ":" -> (reverse acc, ":", us)
      _ -> go (u : acc) us

type ParserT b a = b -> Either Error (b, a)

--------------------------------------------------------------------------------

type PUnits = [Structure2.PUnit]

exprOfPBrack :: Structure2.BracketType -> PUnits -> Either Error Expr
exprOfPBrack Structure2.Round us = sh us
exprOfPBrack Structure2.Scope us = Block <$> parseBlock us
exprOfPBrack p _ = error $ "BUG: exprOfPBrack: " <> show p

-- | It is not a Symbol.
exprOfToken :: Span -> Token -> Either Error Expr
exprOfToken span (Qualid qs n) = pure $ IdE qs n
exprOfToken span (Decimal n) = pure $ NumberE n
exprOfToken span (Literal s) = pure $ LitE s
exprOfToken (Span loc _) tok = Left $ UnexpectedToken loc tok 

mkApp :: [Expr] -> Expr
mkApp [] = error "impossible"
mkApp [e] = e
mkApp (h:tl) = AppE h tl

-- | Shunting yard algorithm.
-- See https://en.wikipedia.org/wiki/Shunting_yard_algorithm .
sh :: PUnits -> Either Error Expr
sh = go [] [] []
  where
    go :: [Expr] -> [FastString] -> [Expr] -> PUnits -> Either Error Expr
    go operands operators cur (Structure2.PBrack btype ins : us) = do
      e' <- exprOfPBrack btype ins
      go operands operators (e':cur) us
    go operands operators cur (Structure2.PToken s@(Span loc _) tok : us) = case tok of
      Symbol name
        | null cur -> Left $ UnexpectedToken loc tok
        | otherwise -> do
            let cur' = mkApp $ reverse cur
            (operands', operators') <- popUntil (fixities name) operators (cur':operands)
            go operands' (name : operators') [] us
            -- go (cur : operands) operators [] us
      _ -> do
        e <- exprOfToken s tok
        go operands operators (e:cur) us
    go operands (name:operators) [] [] = do
      (operands', operators') <- popUntil (fixities name) operators operands
      go operands' operators' [] []
    go [operand] [] [] [] = pure operand
    go operands [] [] [] = error $ "BUG: too many operands: " <> show operands
    go operands operators cur [] = do
      let cur' = mkApp $ reverse cur
      go (cur':operands) operators [] []

popUntil :: Fixity -> [FastString] -> ParserT [Expr] [FastString]
popUntil _ [] operands = pure (operands, [])
popUntil fx (name : operators') operands
  | fx << fixities name = do
      operands' <- mkOp name fx operands
      popUntil fx operators' operands'
popUntil _ operators operands = 
  pure (operands, operators)

mkOp :: FastString -> Fixity -> [Expr] -> Either Error [Expr]
mkOp name (Fixity (Binary _) _) (e2:e1:es) =
  -- fixme! spans are lost ...
  pure $ AppE (IdE [] name) [e1, e2] : es
mkOp name (Fixity Unary _) (e:es) =
  pure $ AppE (IdE [] name) [e] : es
mkOp _name _ [] = Left $ error "mkOp: no operands"
mkOp _name _ [_] = Left $ error "mkOp: not enough operands"
