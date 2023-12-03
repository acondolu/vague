module Main (main) where

import qualified Data.ByteString as ByteString
import System.Environment (getArgs)
import qualified Vague.Lexer as Lexer
import qualified Vague.Parser as Parser

main :: IO ()
main = getArgs >>= \case
  [fp] -> do
    bs <- ByteString.readFile fp
    let res = Parser.parse $ Lexer.lexer bs
    -- let res = Lexer.lexer bs
    print res
  args -> error $ "invalid arguments supplied: " <> show args
