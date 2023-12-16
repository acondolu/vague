module Main (main) where

import qualified Data.ByteString as ByteString
import System.Environment (getArgs)
import qualified Vague.Interpreter as Interpreter
import qualified Vague.Lexer as Lexer
import qualified Vague.Parser as Parser
import qualified Vague.Parser.Error as Error

main :: IO ()
main =
  getArgs >>= \case
    [fp] -> do
      bs <- ByteString.readFile fp
      let res = Parser.parse $ Lexer.lexer bs
      -- let res = Lexer.lexer bs
      case res of
        Left err -> Error.print fp err
        Right program -> Interpreter.run program
    args -> error $ "invalid arguments supplied: " <> show args
