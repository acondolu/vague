module Main (main) where

import qualified Data.ByteString as ByteString
import System.Environment (getArgs)
import qualified Vague.Lexer as Lexer
import qualified Vague.Parser as Parser

main :: IO ()
main = getArgs >>= \case
  [fp] -> do
    bs <- ByteString.readFile fp
    print $ Parser.parse $ Lexer.lexer bs
  args -> error $ "invalid arguments supplied: " <> show args
