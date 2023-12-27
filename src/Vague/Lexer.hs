{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Vague.Lexer
  ( LxStream (..),
    Token (..),
    LxError (..),
    lexer,
  )
where

import qualified Data.Array as Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Function ((&))
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Text.Regex.PCRE as PCRE
import Vague.FastString (FastString)
import qualified Vague.FastString as FastString
import Vague.Located
import Prelude hiding (span)

data LxStream
  = LToken Span Token ~LxStream
  | LError LxError
  | LEnd
  deriving (Show)

data Loose = Loose Bool Bool
  deriving (Show, Eq)

data Token
  = -- brackets
    LCurly
  | RCurly
  | LRound
  | RRound
  | LSquare
  | RSquare
  | LSplice
  | -- scoping (inserted)
    ScopeBegin
  | ScopeEnd
  | -- other symbols
    Symbol FastString Loose
  | --
    Qualid FastString FastString
  | Decimal Integer
  | Literal ByteString
  | -- Keywords
    Keyword FastString
  deriving (Show, Eq)

data LxError
  = LxBug String
  | UnexpectedEOF Loc
  | UnexpectedChar Loc Char
  deriving (Show)

lexerError :: LxError -> LxStream
lexerError = LError

type Action =
  -- | Matching source span
  Span ->
  -- | Matching string
  ByteString ->
  -- | Current state
  State ->
  -- | Stream of tokens
  LxStream

runLexer :: State -> LxStream
runLexer State {ctxStack = []} =
  lexerError $ LxBug "runLexer: empty context stack"
runLexer State {..} | ByteString.null input = doEOF location layouts
runLexer State {ctxStack = s@((pattern, acts) : _), ..} =
  case PCRE.matchOnceText pattern input of
    Nothing -> parseErrorOnInput location input
    Just (_, arr, rest) -> go rest $ tail $ Array.assocs arr
  where
    go _ [] = lexerError $ LxBug "runLexer: match with no capture"
    go rest ((_, ("", _)) : ms)
      | not (null ms) =
          -- Ignore empty strings, as they signal "no match".
          -- Unless it's the last possible action. In fact,
          -- we assume that the last action may have empty
          -- capture, and in that case, we run it (see below).
          go rest ms
    go rest ((i, (m, _)) : _) = case acts Vector.!? (i - 1) of
      Nothing -> lexerError $ LxBug "runLexer: unknown action"
      Just act -> do
        let loc' = incrLoc m location
            isSpace' = isSpace m
        act (Span location loc') m $ State s layouts loc' currentMatchIsSpace isSpace' rest
    incrLoc m (Loc i j)
      -- Careful, relies on the assumption that
      -- newlines are always lexed separately and one
      -- by one:
      | m == "\n" = Loc (i + 1) 1
      | otherwise = Loc i (j + nChars m)

parseErrorOnInput :: Loc -> ByteString -> LxStream
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

isSpace :: ByteString -> Bool
isSpace bs = case PCRE.matchOnceText pat bs of
  Nothing -> False
  Just _ -> True
  where
    pat :: PCRE.Regex
    pat = PCRE.makeRegexOpts PCRE.compUTF8 PCRE.defaultExecOpt ("^\\p{Zs}" :: ByteString)

--------------------------------------------------------------------------------
-- Contexts

data LContextName = L0 | LBOL | LINIT | LTEST | LMAYBELAYOUT | LFINDOFFSIDE
  deriving (Eq, Ord, Show, Enum, Bounded)

type LContext = (PCRE.Regex, Vector.Vector Action)

mkLContext :: [(String, Action)] -> LContext
mkLContext ys =
  let !patstr = "\\A(" <> intercalate ")|\\A(" (map fst ys) <> ")"
      !pattern = PCRE.makeRegexOpts PCRE.compUTF8 PCRE.defaultExecOpt $ pack patstr
      !actions = Vector.fromList $ map snd ys
   in (pattern, actions)

-- See https://www.pcre.org/original/doc/html/pcrepattern.html .
rules :: [([LContextName], String, Action)]
rules =
  [ ([], "(?:(?!\\n)\\p{Zs})+", doSkip), -- skip whitespace but not newlines
    ([], "#[^\\n]*", doSkip), -- skip comments
    -- LBOL
    ([LINIT, LBOL, LFINDOFFSIDE], "\\n", doSkip), -- skip newlines
    ([LBOL], "(?!\\p{Zs})", doBol), -- WARNING! It should be the last rule!
    -- LINIT
    ([LINIT], "(?!\\p{Zs})", doInit), -- WARNING! It should be the last rule!
    -- L0
    ([L0], "\\n", \_ _ -> runLexer . pushLContext LBOL),
    ([L0], "\\{", openBrace),
    ([L0], "\\}", closeBrace),
    ([L0], "\\(", token LRound),
    ([L0], "\\(", token RRound),
    ([L0], "\\[", token LSquare),
    ([L0], "\\]", token RSquare),
    ([L0], "$\\(", token LSplice),
    ([L0], "type", token (Keyword "type")),
    ([L0], "import", token (Keyword "import")),
    ([L0], "(?:" <> varidRe <> "\\.)*" <> varidRe, doId),
    ([L0], "\\-?[0-9]+", doDecimal),
    ([L0], symRe, doSymbol),
    ([L0], "\"", doString),
    -- LMAYBELAYOUT
    -- - If there's a newline, create new layout.
    --   But first, find the offside.
    ([LMAYBELAYOUT], "\\n", \_ _ -> runLexer . pushLContext LFINDOFFSIDE . popLContext),
    -- - If there's no newline, cancel the layout context.
    ([LMAYBELAYOUT], "(?!\\n)", \_ _ -> runLexer . popLContext),
    -- LFINDOFFSIDE
    ([LFINDOFFSIDE], "(?=.)", doFindOffside)
  ]

varidRe :: String
varidRe = "[\\p{L}_][\\p{L}\\p{N}\\p{M}_]*"

symRe :: String
symRe = "[\\!\\$\\%\\&\\*\\+\\.\\/\\<\\=\\>\\?\\@\\\\\\^\\|\\-\\~\\:]+"

-- | Get lexer context from its name.
getLContext :: LContextName -> LContext
getLContext = (Map.!) lmap
  where
    allNames = enumFrom minBound
    !lmap = Map.fromList $ map go allNames
    go s = (s, mkLContext $ map (\(_, str, act) -> (str, act)) $ filter (\(ss, _, _) -> null ss || s `elem` ss) rules)

--------------------------------------------------------------------------------
-- Actions

doId :: Action
doId span match state = do
  let (i, id') = go 0 match 0 match
  let qual = FastString.fromByteString $ ByteString.take i match
      name = FastString.fromByteString id'
  LToken span (Qualid qual name) $ runLexer state
  where
    go i j cur bs = case UTF8.decode bs of
      Nothing -> (i, j)
      Just ('.', n) -> do
        let bs' = ByteString.drop n bs
        go cur bs' (cur + n) bs'
      Just (_, n) -> go i j (cur + n) (ByteString.drop n bs)

doSymbol :: Action
doSymbol span "=" state = do
  let tok = Keyword "="
  LToken span tok $ runLexer $ pushLContext LMAYBELAYOUT state
doSymbol span ":" state = do
  let tok = Keyword ":"
  LToken span tok $ runLexer state
doSymbol span match state = do
  let tok = Symbol (FastString.fromByteString match) (loose state)
      state' = state -- maybeLayout match state
  LToken span tok $ runLexer state'

loose :: State -> Loose
loose State {..} = do
  let before = previousMatchWasSpace
      after = isSpace input
  Loose before after

-- maybeLayout :: ByteString -> State -> State
-- maybeLayout "=" = pushLContext LMAYBELAYOUT
-- maybeLayout _ = id

doDecimal :: Action
doDecimal span match state = do
  let s = UTF8.toString match
      tok = Decimal (read s)
  LToken span tok $ runLexer state

doString :: Action
doString (Span b e) _match State {..} =
  case go 0 [] False input of
    Left len' -> do
      let e' = case e of Loc i j -> Loc i (j + len')
      lexerError $ UnexpectedEOF e'
    Right (len', str', input') -> do
      let e' = case e of Loc i j -> Loc i (j + len')
      LToken (Span b e') (Literal $ UTF8.fromString str') $ runLexer State {input = input', ..}
  where
    go ::
      Int ->
      -- \^ Number of bytes consumed
      String ->
      -- \^ Parsed string (reversed!)
      Bool ->
      -- \^ If last char was \\ (escape)
      ByteString ->
      -- \^ Input
      Either Int (Int, String, ByteString)
    go !len str escaping !bs
      | Just (c, i) <- UTF8.decode bs = do
          let bs' = ByteString.drop i bs
              len' = len + 1
          case c of
            '"' ->
              if escaping
                then go len' ('"' : str) False bs'
                else Right (len', reverse str, bs')
            '\\' ->
              if escaping
                then go len' ('\\' : str) False bs'
                else go len' str True bs'
            _
              | escaping ->
                  case Map.lookup c escapeCodes of
                    Just c' -> go len' (c' : str) False bs'
                    Nothing -> go len' (c : '\\' : str) False bs'
              | otherwise ->
                  go len' (c : str) False bs'
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

-- | Skip matched string.
doSkip :: Action
doSkip _span _match = runLexer

-- | Emit token.
token :: Token -> Action
token t sp _match state = LToken sp t $ runLexer state

-- | First token on a line, insert layout tokens if necessary.
doBol :: Action
doBol span _match state = do
  let col = case span of Span _ (Loc _ col') -> col'
      loc = case span of Span _ loc' -> loc'
      state' = popLContext state
      maybePopLayouts []
        | col == 1 = LToken (Span loc loc) (Keyword ";") $ runLexer (state' {layouts = []})
        | otherwise = runLexer (state' {layouts = []})
      maybePopLayouts l@(Layout n : ls) =
        case compare n col of
          LT -> runLexer (state' {layouts = l})
          EQ -> LToken (Span loc loc) (Keyword ";") $ runLexer (state' {layouts = l})
          GT -> LToken (Span loc loc) ScopeEnd $ maybePopLayouts ls
      maybePopLayouts (NoLayout : _) = error "doBol: NoLayout" -- TODO: remove NoLayout
  maybePopLayouts (layouts state)

doInit :: Action
doInit _ _ = runLexer . popLContext

openBrace :: Action
openBrace sp _match state =
  LToken sp LCurly $ runLexer (pushLayout NoLayout state)

closeBrace :: Action
closeBrace sp match state =
  case popLayout state of
    Nothing -> case sp of
      Span loc _ -> parseErrorOnInput loc match
    Just state' -> LToken sp RCurly $ runLexer state'

doFindOffside :: Action
doFindOffside span _ state = do
  let col = case span of Span _ (Loc _ col') -> col'
      state' = state & pushLayout (Layout col) & popLContext
  LToken span ScopeBegin $ runLexer state'

doEOF :: Loc -> [Layout] -> LxStream
doEOF loc = popAllLayouts
  where
    popAllLayouts [] = LEnd
    popAllLayouts (Layout _ : ls) = LToken (Span loc loc) ScopeEnd $ popAllLayouts ls
    popAllLayouts (NoLayout : _) = error "doEOF: NoLayout" -- TODO: remove NoLayout

--------------------------------------------------------------------------------
-- Lexer state

data State = State
  { ctxStack :: [LContext],
    layouts :: [Layout],
    location :: Loc,
    previousMatchWasSpace :: Bool,
    currentMatchIsSpace :: Bool,
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
lexer :: ByteString -> LxStream
lexer input = do
  let initCtxStack = [getLContext LINIT, getLContext L0]
      loc = Loc 1 1
      initState = State initCtxStack [] loc True True
  runLexer $ initState input
