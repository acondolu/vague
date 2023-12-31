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

newtype ModuleName = ModuleName [Id] 
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

-- TODO: extend to values (constructors, ...)
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
  | SpliceE [Statement]
  | QuoteE Id
  deriving (Show)

data TypeExpr
  = TypeAtom Text
  | TypeUnion TypeExpr TypeExpr
  | TypeRecord [(Id, TypeExpr)]
  deriving (Show)
