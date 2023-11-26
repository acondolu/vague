module Main (main) where

import Lib
-- import Vague.Parser (pProgram)
-- import Data.Attoparsec.Text (parseOnly)
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

main :: IO ()
main = do
  error "TODO"
  -- bs <- readFile "test.txt"
  -- case parseOnly pProgram bs of
  --   Left err -> error err
  --   Right _ -> print "OK"
