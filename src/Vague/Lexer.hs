{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Vague.Lexer (LStream (..), Token (..), LexerError (..), main, lexer) where

import qualified Data.Array as Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Char8 (pack)
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.UTF8 as UTF8
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Debug.Trace
import qualified Text.Regex.PCRE as PCRE
import Vague.FastString (FastString)
import qualified Vague.FastString as FastString
import Vague.Located
import Prelude hiding (span)
import Data.Function ((&))

-- https://www.pcre.org/original/doc/html/pcrepattern.html

data LStream
  = LToken Span Token LStream
  | LError LexerError
  | LEnd
  deriving (Show)

data Token
  = LCurly
  | RCurly
  | LParens
  | RParens
  | LBracket
  | RBracket
  | Comma
  | Semicolon
  | Qualid [FastString] FastString
  | Symbol FastString
  | Decimal Integer
  | Literal ByteString
  | LScope
  | LScopeEnd
  | LSep
  | LOL -- just for testing
  deriving (Show)

data LexerError
  = LexerError String
  | UnexpectedEOF Loc
  | UnexpectedChar Loc Char
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
  lexerError $ LexerError "BUG: runLexer: empty context stack"
runLexer State {..} | ByteString.null input = eof location layouts
runLexer State {ctxStack = s@((pattern, acts) : _), ..} =
  case PCRE.matchOnceText pattern input of
    Nothing -> parseErrorOnInput location input
    Just (_, arr, rest) -> go rest $ tail $ Array.assocs arr
  where
    go _ [] = lexerError $ LexerError "BUG: runLexer: match with no capture"
    go rest ((_, ("", _)) : ms) | not (null ms) = go rest ms
    go rest ((i, (m, _)) : _) = case acts Vector.!? (i - 1) of
      Nothing -> lexerError $ LexerError "BUG: runLexer: unknown action"
      Just act -> do
        let loc' = incrLoc m location
        act (Span location loc') m $ State s layouts loc' rest
    incrLoc m (Loc i j)
      | m == "\n" -- careful, newlines must be lexed separately
        =
          Loc (i + 1) 1
      | otherwise = Loc i (j + nChars m)

parseErrorOnInput :: Loc -> ByteString -> LStream
parseErrorOnInput loc bs = lexerError $ case UTF8.decode bs of
    Nothing -> UnexpectedEOF loc
    Just (c, _) -> UnexpectedChar loc c
  -- show loc <> ": parse error" <> case UTF8.decode bs of
  --   Nothing -> []
  --   Just (c, _) -> " on input `" <> [c] <> "`"

-- | Get the number of unicode characters in a UTF8 byte stream.
nChars :: ByteString -> Int
nChars "" = 0
nChars bs = case UTF8.decode bs of
  Nothing -> 0
  Just (_, n) -> 1 + nChars (ByteString.drop n bs)

--------------------------------------------------------------------------------
-- Contexts

data LContextName = L0 | LBOL | LTEST | LLAYOUT | LFINDOFFSIDE
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
  [ ([], "(?:(?!\\n)\\p{Zs})+", skip), -- skip whitespace but not newlines
    ([], "#[^\\n]*", skip), -- skip comments
    -- LBOL
    ([LBOL, LFINDOFFSIDE], "\\n", skip), -- skip newlines
    ([LBOL], "(?!\\p{Zs})", doBol), -- WARNING! It should be the last rule!
    -- L0
    ([L0], "\\n", \_ _ -> runLexer . pushLContext LBOL),
    ([L0], "\\{", openBrace),
    ([L0], "\\}", closeBrace),
    ([L0], "\\(", token LParens),
    ([L0], "\\(", token RParens),
    ([L0], "\\[", token LBracket),
    ([L0], "\\]", token RBracket),
    ([L0], ",", token Comma),
    ([L0], ";", token Semicolon),
    ([L0], "(?:" <> varidRe <> "\\.)*" <> varidRe, idtoken),
    ([L0], "\\-?[0-9]+", decimal),
    ([L0], symRe, symtoken),
    ([L0], "\"", lexString),
    -- LLAYOUT
    -- - If there's a newline, insert a new layout.
    --   But first, find the offside.
    ([LLAYOUT], "\\n", \_ _ -> runLexer . pushLContext LFINDOFFSIDE . popLContext),
    -- - If there's no newline, cancel the layout context.
    ([LLAYOUT], "(?!\\n)", \_ _ -> runLexer . popLContext),
    -- LFINDOFFSIDE
    ([LFINDOFFSIDE], "(?=.)", lexFindOffside),
    -- TESTING (to remove)
    ([LTEST], "\\x{1F923}", token LOL)
  ]

varidRe :: String
varidRe = "[\\p{L}_][\\p{L}\\p{N}\\p{M}_]*"

symRe :: String
symRe = "[\\!\\$\\%\\&\\*\\+\\.\\/\\<\\=\\>\\?\\@\\\\\\^\\|\\-\\~\\:]+"

-- | Get runLexer context from its name.
getLContext :: LContextName -> LContext
getLContext = (Map.!) lmap
  where
    allNames = enumFrom minBound
    !lmap = Map.fromList $ map go allNames
    go s = (s, mkLContext $ map (\(_, str, act) -> (str, act)) $ filter (\(ss, _, _) -> null ss || s `elem` ss) rules)

--------------------------------------------------------------------------------
-- Actions

idtoken :: Action
idtoken span match state = do
  let ids =
        map FastString.fromByteString $
          -- FIXME: do not split on the byte '.'
          -- which is wrong according to unicode
          ByteString.split (c2w '.') match
  case unsnoc ids of
    Nothing -> lexerError $ LexerError "BUG: idtoken"
    Just (xs, x) -> do
      let tok = Qualid xs x
      LToken span tok $ runLexer state
  where
    unsnoc [] = Nothing
    unsnoc (x : xs) = Just (go x xs)
    go x [] = ([], x)
    go x (y : ys) = do
      let (zs, z) = go y ys
      (x : zs, z)

symtoken :: Action
symtoken span match state = do
  let tok = Symbol (FastString.fromByteString match)
      state' = maybeLayout match state
  LToken span tok $ runLexer state'

maybeLayout :: ByteString -> State -> State
maybeLayout "=" = pushLContext LLAYOUT
maybeLayout _ = id

decimal :: Action
decimal span match state = do
  let s = UTF8.toString match
      tok = Decimal (read s)
  LToken span tok $ runLexer state

lexString :: Action
lexString (Span b e) _match State {..} =
  case go 0 [] False input of
    Left len' -> do
      let e' = case e of Loc i j -> Loc i (j + len')
      lexerError $ UnexpectedEOF e'
    Right (len', str', input') -> do
      let e' = case e of Loc i j -> Loc i (j + len')
      LToken (Span b e') (Literal $ UTF8.fromString str') $ runLexer State {input = input', ..}
  where
    go !len str escaping !bs
      | Just (c, i) <- UTF8.decode bs = do
          let bs' = ByteString.drop i bs
          case c of
            '"' ->
              if escaping
                then go (len + 1) ('"' : str) False bs'
                else Right (len + 1, reverse str, bs')
            '\\' ->
              if escaping
                then go (len + 1) ('\\' : str) False bs'
                else go (len + 1) str True bs'
            _
              | escaping,
                Just c' <- Map.lookup c escapeCodes ->
                  go (len + 1) (c' : str) False bs'
            _ -> go (len + 1) (c : str) False bs'
      | otherwise = Left len

escapeCodes :: Map.Map Char Char
escapeCodes =
  Map.fromList
    [ ('0', '\0'),
      ('a', '\a'),
      ('b', '\b'),
      ('f', '\f'),
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t'),
      ('v', '\v')
    ]

-- case UTF8.decode bs of
--   Nothing -> 0
--   Just (_, n) -> 1 + nChars (ByteString.drop n bs)

-- | Skip matched string.
skip :: Action
skip _span _match = runLexer

-- | Emit token.
token :: Token -> Action
token t sp _match state = LToken sp t $ runLexer state

-- | First token on a line, insert layout tokens if necessary.
doBol :: Action
doBol span _match state = do
  let col = case span of { Span _ (Loc _ col') -> col' }
      loc = case span of { Span _ loc' -> loc' }
      state' = popLContext state
      maybePopLayouts [] = runLexer (state' {layouts=[]})
      maybePopLayouts l@(Layout n:ls) =
        case compare n col of
          LT -> runLexer (state' {layouts=l})
          EQ -> LToken (Span loc loc) LSep $ runLexer (state' {layouts=l})
          GT -> LToken (Span loc loc) LScopeEnd $ maybePopLayouts ls
      maybePopLayouts (NoLayout:_) = error "doBol: NoLayout" -- TODO: remove NoLayout
  maybePopLayouts (layouts state)

openBrace :: Action
openBrace sp _match state =
  LToken sp LCurly $ runLexer (pushLayout NoLayout state)

closeBrace :: Action
closeBrace sp match state =
  case popLayout state of
    Nothing -> case sp of
      Span loc _ -> parseErrorOnInput loc match
    Just state' -> LToken sp RCurly $ runLexer state'

lexFindOffside :: Action
lexFindOffside span _ state = do
  let col = case span of { Span _ (Loc _ col') -> col' }
      state' = state & pushLayout (Layout col) & popLContext
  LToken span LScope $ runLexer state'

eof :: Loc -> [Layout] -> LStream
eof loc = popAllLayouts
  where
    popAllLayouts [] = LEnd
    popAllLayouts (Layout _:ls) = LToken (Span loc loc) LScopeEnd $ popAllLayouts ls
    popAllLayouts (NoLayout:_) = error "eof: NoLayout" -- TODO: remove NoLayout

--------------------------------------------------------------------------------
-- Lexer state

data State = State
  { ctxStack :: [LContext],
    layouts :: [Layout],
    location :: Loc,
    input :: ByteString
  }

data Layout
  = NoLayout
  | Layout Int
  deriving (Show)

pushLayout :: Layout -> State -> State
pushLayout l State {..} = State {layouts = l : layouts, ..}

popLayout :: State -> Maybe State
popLayout State {..} = case layouts of
  [] -> Nothing
  _ : ls -> Just State {layouts = ls, ..}

pushLContext :: LContextName -> State -> State
pushLContext s State {..} = State {ctxStack = getLContext s : ctxStack, ..}

popLContext :: State -> State
popLContext State {..} = case ctxStack of
  [] -> error "BUG: popLContext"
  _ : cs -> State {ctxStack = cs, ..}

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
  print PCRE.configUTF8
  let initCtxStack = [getLContext LTEST, getLContext LBOL, getLContext L0]
      loc = Loc 1 1
      initState = State initCtxStack [] loc
  print $ runLexer $ initState $ UTF8.fromString "{ { }     🤣 # this is a comment\n }"
