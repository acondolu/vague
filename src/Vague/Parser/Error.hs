module Vague.Parser.Error (Error (..), fromLexerError) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Vague.Lexer as Lexer
import Vague.Located (Loc)

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
