module Vague.Parser.Syntax where

import Data.Text (Text)
import Vague.FastString (FastString)
import Data.ByteString (ByteString)

newtype Program = Program [Statement]
  deriving (Show)

data Import
  = ImQualified [Id] Id
  | ImFrom [Id] Id [Id]

data Statement
  = -- | id: TypeExpr
    TypeDeclaration Id TypeExpr
  | -- | type id = TypeExpr
    TypeBinding Id TypeExpr
  | -- | x = Expr
    Binding Pattern Expr
  | Statement Expr
  deriving (Show)

type Pattern = [Id]

type Id = FastString

data Expr
  = NumberE Integer
  | FloatE Double
  | LitE ByteString
  | IdE [Id] Id
  | AppE Expr [Expr]
  | FunE [Id] Expr
  | Block [Statement]
  deriving (Show)

data TypeExpr
  = TypeAtom Text
  | TypeUnion TypeExpr TypeExpr
  | TypeRecord [(Id, TypeExpr)]
  deriving (Show)
