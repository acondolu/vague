{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Vague.Lexer (LStream (..), Token (..), LexerError (..), main, lexer) where

import qualified Data.Array as Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.UTF8 as UTF8
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Debug.Trace
import qualified Text.Regex.PCRE as PCRE
import Vague.Located

-- https://www.pcre.org/original/doc/html/pcrepattern.html

data LStream
  = LToken Span Token LStream
  | LError LexerError
  | LEnd
  deriving (Show)

data Token
  = LCurly
  | RCurly
  | LOL -- just for testing
  deriving (Show)

data LexerError = LexerError String
  deriving (Show)

lexerError :: LexerError -> LStream
lexerError = LError

type Action =
  -- | The match
  Span ->
  ByteString ->
  State ->
  LStream

runLexer :: State -> LStream
runLexer State {ctxStack = []} =
  lexerError $ LexerError "BUG: empty context stack"
runLexer State {..} | ByteString.null input = LEnd -- end of file
runLexer State {ctxStack = s@((pattern, acts) : _), ..} =
  case PCRE.matchOnceText pattern input of
    Nothing -> lexerError $ LexerError $ parseErrorOnInput location input
    Just (_, arr, rest) -> go rest $ tail $ Array.assocs arr
  where
    go _ [] = lexerError $ LexerError "bug: match with no capture"
    go rest ((_, ("", _)) : ms) | not (null ms) = go rest ms
    go rest ((i, (m, _)) : _) = trace (show (i, m)) $ case acts Vector.!? (i - 1) of
      Nothing -> lexerError $ LexerError "bug: unknown action"
      Just act -> do
        let loc' = incrLoc m location
        act (Span location loc') m $ State s layouts loc' rest
    incrLoc m (Loc i j)
      | m == "\\n" -- careful, newlines must be lexed separately
        =
          Loc (i + 1) j
      | otherwise = Loc i (j + nChars m)

parseErrorOnInput :: Loc -> ByteString -> String
parseErrorOnInput loc bs =
  show loc <> ": parse error" <> case UTF8.decode bs of
    Nothing -> []
    Just (c, _) -> " on input `" <> [c] <> "`"

-- | Get the number of unicode characters in a UTF8 byte stream.
nChars :: ByteString -> Int
nChars "" = 0
nChars bs = case UTF8.decode bs of
  Nothing -> 0
  Just (_, n) -> 1 + nChars (ByteString.drop n bs)

--------------------------------------------------------------------------------
-- Contexts

data LContextName = L0 | LBOL | LTEST
  deriving (Eq, Ord, Show, Enum, Bounded)

type LContext = (PCRE.Regex, Vector.Vector Action)

mkLContext :: [(String, Action)] -> LContext
mkLContext ys =
  let !patstr = "\\A(" <> intercalate ")|\\A(" (map fst ys) <> ")"
      !pattern = PCRE.makeRegexOpts PCRE.compUTF8 PCRE.defaultExecOpt $ pack patstr
      !actions = Vector.fromList $ map snd ys
   in (pattern, actions)

rules :: [([LContextName], String, Action)]
rules =
  [ ([], "(?:(?!\\n)\\p{Zs})+", skip) -- skip whitespace but not newlines
  , ([], "#[^\\n]*", skip) -- skip comments
  , ([LBOL], "\\n", skip) -- skip newlines
  , ([LBOL], "(?!\\p{Zs})", doBol) -- WARNING! It should be the last rule!
  , ([L0], "\\n", \_ _ -> runLexer . pushLContext LBOL)
  , ([L0], trace "L0{" "\\{", openBrace)
  , ([L0], "\\}", closeBrace)
  , ([LTEST], "\\x{1F923}", token LOL)
  ]

-- | Get runLexer context from its name.
getLContext :: LContextName -> LContext
getLContext = (Map.!) lmap
  where
    allNames = enumFrom minBound
    !lmap = Map.fromList $ map go allNames
    go s = (s, mkLContext $ map (\(_, str, act) -> (str, act)) $ filter (\(ss, _, _) -> null ss || s `elem` ss) rules)

--------------------------------------------------------------------------------
-- Actions

-- | Skip matched string.
skip :: Action
skip _span _match state = runLexer state

-- | Emit token.
token :: Token -> Action
token t sp _match state = LToken sp t $ runLexer state

-- | First token on a line, insert layout tokens if necessary.
doBol :: Action
doBol _span _match state = runLexer (popLContext state)

openBrace :: Action
openBrace sp _match state =
  LToken sp LCurly $ runLexer (pushLayout NoLayout state)

closeBrace :: Action
closeBrace sp match state =
  case popLayout state of
    Nothing -> case sp of
      Span loc _ -> lexerError $ LexerError $ parseErrorOnInput loc match
    Just state' -> LToken sp RCurly $ runLexer state'

--------------------------------------------------------------------------------
-- Lexer state

data State = State
  { ctxStack :: [LContext]
  , layouts :: [Layout]
  , location :: Loc
  , input :: ByteString
  }

data Layout
  = NoLayout
  | Layout Int Bool
  deriving (Show)

pushLayout :: Layout -> State -> State
pushLayout l State {..} = State {layouts = l : layouts, ..}

popLayout :: State -> Maybe State
popLayout State {..} = case layouts of
  [] -> Nothing
  _:ls -> Just State {layouts=ls, ..}

pushLContext :: LContextName -> State -> State
pushLContext s State {..} = State {ctxStack = getLContext s : ctxStack, ..}

popLContext :: State -> State
popLContext State {..} = case ctxStack of
  [] -> error "BUG: popLContext"
  _:cs -> State {ctxStack=cs, ..}

--------------------------------------------------------------------------------

-- | Lex the input UTF-8 encoded bytestring.
lexer :: ByteString -> LStream
lexer input = do
  let initCtxStack = [getLContext LBOL, getLContext L0]
      loc = Loc 1 1
      initState = State initCtxStack [] loc
  runLexer $ initState input

main :: IO ()
main = do
  print $ PCRE.configUTF8
  let initCtxStack = [getLContext LTEST, getLContext LBOL, getLContext L0]
      loc = Loc 1 1
      initState = State initCtxStack [] loc
  print $ runLexer $ initState $ UTF8.fromString "{ { }     🤣 # this is a comment\n }"
