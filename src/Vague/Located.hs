{-# LANGUAGE StrictData #-}

module Vague.Located
  ( Loc (..),
    Span (..),
    Located (..),
  )
where

data Loc = Loc {locRow :: Int, locCol :: Int}

instance Show Loc where
  show (Loc row col) = show row <> ":" <> show col

data Span = Span Loc Loc
  deriving (Show)

instance Semigroup Span where
  Span loc1 _ <> Span _ loc2 = Span loc1 loc2

data Located a = Located Span a
  deriving (Show)
