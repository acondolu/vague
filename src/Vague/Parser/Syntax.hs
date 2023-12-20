module Vague.Parser.Syntax where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Vague.FastString (FastString)

data Program = Program [Import] [Statement]
  deriving (Show)

newtype Import
  = ImQualified ModuleName
  -- ImFrom [Id] Id [Id]
  deriving (Show)

data ModuleName = ModuleName [Id] Id
  deriving (Show)

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
  | IdE FastString Id
  | AppE Expr [Expr]
  | FunE [Id] Expr
  | Block [Statement]
  | RecordE [(Pattern, Expr)]
  deriving (Show)

data TypeExpr
  = TypeAtom Text
  | TypeUnion TypeExpr TypeExpr
  | TypeRecord [(Id, TypeExpr)]
  deriving (Show)
