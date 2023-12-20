{-# LANGUAGE OverloadedStrings #-}

module Vague.Interpreter (run) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Vague.FastString (FastString)
import qualified Vague.Parser.Syntax as Parser

type Scope = Map FastString (IORef Value)

mkScope :: [Parser.Statement] -> IO Scope
mkScope = aux mempty
  where
    aux s (Parser.Binding [id] expr : ds) = do
      v <- runExpr s expr
      ref <- newIORef v
      aux (Map.insert id ref s) ds
    aux s (_ : ds) = aux s ds
    aux s [] = pure s

run :: Parser.Program -> IO ()
run (Parser.Program imports decls) = do
  scope <- mkScope decls
  case Map.lookup "main" scope of
    Nothing -> error "main function is not defined"
    Just expr -> do
      main <- readIORef expr
      case main of
        FunV f -> void $ f VoidV
        _ -> error "main is not a function"

data Value
  = NumberV Integer
  | FloatV Double
  | LitV ByteString
  | FunV (Value -> IO Value)
  | VoidV

builtins :: Map FastString Value
builtins =
  Map.fromList
    [ ("putStrLn", FunV $ putStrLn')
    ]
  where
    putStrLn' (LitV s) = print s *> pure VoidV
    putStrLn' _ = error "putStrLn: expected string"

runExpr :: Scope -> Parser.Expr -> IO Value
runExpr _ (Parser.NumberE n) = pure $ NumberV n
runExpr _ (Parser.FloatE n) = pure $ FloatV n
runExpr _ (Parser.LitE s) = pure $ LitV s
runExpr scope (Parser.IdE _ id) =
  case Map.lookup id scope of
    Nothing -> case Map.lookup id builtins of
      Nothing -> error $ "name " <> show id <> " is not defined"
      Just b -> pure b
    Just e -> readIORef e
runExpr scope (Parser.AppE h tl) = do
  h' <- runExpr scope h
  tl' <- traverse (runExpr scope) tl
  mkApps h' tl'
  where
    mkApps :: Value -> [Value] -> IO Value
    mkApps h' [] = pure h'
    mkApps h' (x : xs) = do
      h'' <- mkApp h' x
      mkApps h'' xs
    mkApp :: Value -> Value -> IO Value
    mkApp (FunV f) b = f b
    mkApp _ _ = error "not callable"
runExpr scope (Parser.FunE xs expr) =
  go scope xs expr
  where
    go scope [] expr = runExpr scope expr
    go scope (x : xs) expr = pure $ FunV $ \y -> do
      ref <- newIORef y
      go (Map.insert x ref scope) xs expr
