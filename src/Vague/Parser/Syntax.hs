module Vague.Parser.Syntax where

import Data.Text (Text)

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
    Binding Id Expr
  | Statement Expr
  deriving (Show)

type Id = Text

data Expr
  = NumberE Int
  | FloatE Double
  | LitE Text
  | IdE Id
  | AppE Expr [Expr]
  | FunE [Id] Expr
  deriving (Show)

data TypeExpr
  = TypeAtom Text
  | TypeUnion TypeExpr TypeExpr
  | TypeRecord [(Id, TypeExpr)]
  deriving (Show)
