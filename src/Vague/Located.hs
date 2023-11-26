{-# LANGUAGE StrictData #-}

module Vague.Located where

data Loc = Loc {locRow :: Int, locCol :: Int}

instance Show Loc where
  show (Loc row col) = show row <> ":" <> show col

data Span = Span Loc Loc
  deriving (Show)
