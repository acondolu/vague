module Vague.Parser.Error (Error (..), fromLexerError, print) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Vague.Lexer as Lexer
import Vague.Located (Loc (..))
import Prelude hiding (print)

data Error
  = Bug Text
  | UnexpectedEOF
  | UnexpectedChar Loc Char
  | ExpectedChar Loc Char
  | UnexpectedToken Loc Lexer.Token
  | ExpectedToken Lexer.Token Loc Lexer.Token
  deriving (Show)

fromLexerError :: Lexer.LexerError -> Error
fromLexerError (Lexer.LexerError str) = Bug (Text.pack str)
fromLexerError Lexer.UnexpectedEOF {} = UnexpectedEOF
fromLexerError (Lexer.UnexpectedChar loc chr) = UnexpectedChar loc chr

errorLoc :: Error -> Maybe Loc
errorLoc Bug {} = Nothing
errorLoc UnexpectedEOF = Nothing
errorLoc (UnexpectedChar loc _) = Just loc
errorLoc (ExpectedChar loc _) = Just loc
errorLoc (UnexpectedToken loc _) = Just loc
errorLoc (ExpectedToken _ loc _) = Just loc

errorMsg :: Error -> String
errorMsg (Bug msg) = "BUG: " <> Text.unpack msg
errorMsg UnexpectedEOF = "Syntax error: unexpected end of file"
errorMsg (UnexpectedChar _ _) = "Syntax error: unexpected character"
errorMsg (ExpectedChar _ c) = "Syntax error: expected `" <> [c] <> "`"
errorMsg (UnexpectedToken _ _) = "Syntax error: invalid syntax"
errorMsg (ExpectedToken _ _ tok) = "Syntax error: expected `" <> show tok <> "`"

print :: FilePath -> Error -> IO ()
print fp err = do
  bs <- readFile fp
  putStr $ "  File " <> show fp
  case errorLoc err of
    Nothing -> putStrLn ""
    Just (Loc row col) -> do
      putStrLn $ ", line " <> show row
      case getLineFrom (row - 1) bs of
        Nothing -> pure ()
        Just line -> do
          putStr "    "
          putStrLn line
          putStrLn $ "    " <> replicate (col - 1) ' ' <> "^"

  putStrLn $ errorMsg err

getLineFrom :: Int -> String -> Maybe String
getLineFrom n str = lines str !? n

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x : xs) !? n
  | n == 0 = Just x
  | otherwise = xs !? (n - 1)
