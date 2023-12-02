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

parseProgram :: Structure.Tree -> Either Error Program
parseProgram (Structure.Leaf []) = Right $ Program []
parseProgram (Structure.TBin span ";" stmt stmts) = do
  stmt <- parseStmt stmt
  Program stmts <- parseProgram stmts
  pure $ Program (stmt : stmts)
parseProgram t = do
  stmt <- parseStmt t
  pure $ Program [stmt]

parseStmt :: Structure.Tree -> Either Error Statement
parseStmt = undefined
