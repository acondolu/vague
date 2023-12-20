module Main (main) where

import Control.Monad (join)
import qualified Data.ByteString as ByteString
import Options.Applicative
import qualified Vague.Interpreter as Interpreter
import qualified Vague.Lexer as Lexer
import qualified Vague.Parser as Parser
import qualified Vague.Parser.Error as Error

main :: IO ()
main = join $ execParser opts
  where
    opts = info (commands <**> helper) fullDesc

commands :: Parser (IO ())
commands = sample <$> argument str (metavar "FILE")

sample :: FilePath -> IO ()
sample fp = do
  bs <- ByteString.readFile fp
  let res = Parser.parse $ Lexer.lexer bs
  -- let res = Lexer.lexer bs
  case res of
    Left err -> Error.print fp err
    Right program -> Interpreter.run program
