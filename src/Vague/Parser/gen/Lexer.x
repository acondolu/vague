-- -----------------------------------------------------------------------------
-- Alex "Haskell code fragment top"

{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- {-# OPTIONS_GHC -DALEX_GHC #-}

module Vague.Parser.Lexer (
   Token(..),
   PState (..), initParserState,
   P(..), ParseResult(..),
   allocateComments,
   getRealSrcLoc, getPState,
   failMsgP, failLocMsgP, srcParseFail,
   getErrorMessages,
   popContext, pushModuleContext, setLastToken,
   nextIsEOF,
   getLexState, popLexState, pushLexState,
   commentToAnnotation,
   lexTokenStream,
) where

-- import GHC.Prelude

-- base
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Word

-- bytestring
import Data.ByteString (ByteString)

-- containers
import Data.Map (Map)
import qualified Data.Map as Map

-- compiler
import Vague.Parser.Located
import Vague.FastString
-- import GHC.Data.Bag
-- import GHC.Utils.Outputable
-- import GHC.Utils.Panic
-- import GHC.Data.StringBuffer
-- import GHC.Data.FastString
-- import GHC.Types.Unique.FM
-- import GHC.Data.Maybe
-- import GHC.Data.OrdList
-- import GHC.Utils.Misc ( readRational, readHexRational )

-- import GHC.Types.SrcLoc
-- import GHC.Unit.Types
-- import GHC.Types.Basic ( InlineSpec(..), RuleMatchInfo(..),
--                          IntegralLit(..), FractionalLit(..),
--                          SourceText(..) )
-- import GHC.Hs.Doc

-- import GHC.Parser.CharClass

-- import GHC.Parser.Annotation
-- import GHC.Driver.Flags
-- import GHC.Parser.Errors
}

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

$unispace    = \x05 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$nl          = [\n\r\f]
$whitechar   = [$nl\v\ $unispace]
$white_no_nl = $whitechar # \n -- TODO #8424
$tab         = \t

$ascdigit  = 0-9
$unidigit  = \x03 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$decdigit  = $ascdigit -- for now, should really be $digit (ToDo)
$digit     = [$ascdigit $unidigit]

$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol = \x04 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$symbol    = [$ascsymbol $unisymbol] # [$special \_\"\']

$unilarge  = \x01 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$asclarge  = [A-Z]
$large     = [$asclarge $unilarge]

$unismall  = \x02 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$ascsmall  = [a-z]
$small     = [$ascsmall $unismall \_]

$unigraphic = \x06 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$graphic   = [$small $large $symbol $digit $special $unigraphic \"\']

$binit     = 0-1
$octit     = 0-7
$hexit     = [$decdigit A-F a-f]

$uniidchar = \x07 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$idchar    = [$small $large $digit $uniidchar \']

-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"

@varid     = [$small $large] $idchar* -- variable identifiers

@varsym    = ($symbol # \:) $symbol*  -- variable (operator) symbol
@consym    = \: $symbol*              -- constructor (operator) symbol

@decimal      = $decdigit $decdigit*
@binary       = $binit binit*
@octal        = $octit $octit*
@hexadecimal  = $hexit $hexit*
@exponent     = [eE] [\-\+]? @decimal
@bin_exponent = [pP] [\-\+]? @decimal

@qual = (@varid \.)+
@qvarid = @qual @varid
@qvarsym = @qual @varsym
@qconsym = @qual @consym

-- QualifiedDo needs to parse "M.do" not as a variable, so as to keep the
-- layout rules.
@qdo    = @qual "do"
@qmdo   = @qual "mdo"

@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent
@hex_floating_point = @hexadecimal \. @hexadecimal @bin_exponent? | @hexadecimal @bin_exponent

-- normal signed numerical literals can only be explicitly negative,
-- not explicitly positive (contrast @exponent)
@negative = \-


-- -----------------------------------------------------------------------------
-- Alex "Identifier"

haskell :-


-- -----------------------------------------------------------------------------
-- Alex "Rules"

-- everywhere: skip whitespace
$white_no_nl+ ;
$tab          { warnTab }

-- line comments
"# " .* { lineCommentToken }

-- 'bol' state: beginning of a line.  Slurp up all the whitespace (including
-- blank lines) until we find a non-whitespace character, then do layout
-- processing.
<bol> {
  \n                                    ;
  ()                                    { do_bol }
}

<layout, layout_do, layout_if> {
  \{                                    { hopefully_open_brace }
  \n                                    ;
}

-- after an 'if', a vertical bar starts a layout context for MultiWayIf
<layout_if> {
  \| / { notFollowedBySymbol }          { new_layout_context True dontGenerateSemic ITvbar }
  ()                                    { pop }
}

-- do is treated in a subtly different way, see new_layout_context
<layout>    ()                          { new_layout_context True  generateSemic ITvocurly }
<layout_do> ()                          { new_layout_context False generateSemic ITvocurly }

-- after a new layout context which was found to be to the left of the
-- previous context, we have generated a '{' token, and we now need to
-- generate a matching '}' token.
<layout_left>  ()                       { do_layout_left }

<0,option_prags> \n                     { begin bol }

-- '0' state: ordinary lexemes

-- Haddock comments

-- "special" symbols

<0,option_prags> {
  \(                                    { special IToparen }
  \)                                    { special ITcparen }
  \[                                    { special ITobrack }
  \]                                    { special ITcbrack }
  \,                                    { special ITcomma }
  \;                                    { special ITsemi }
  \`                                    { special ITbackquote }

  \{                                    { open_brace }
  \}                                    { close_brace }
}

<0,option_prags> {
  @qdo                                      { qdo_token ITdo }
  @qvarid                       { idtoken qvarid }
  @varid                        { varid }
}

-- Operators classified into prefix, suffix, tight infix, and loose infix.
-- See Note [Whitespace-sensitive operator parsing]
<0> {
  @varsym / { precededByClosingToken `alexAndPred` followedByOpeningToken } { varsym_tight_infix }
  @varsym / { followedByOpeningToken }  { varsym_prefix }
  @varsym / { precededByClosingToken }  { varsym_suffix }
  @varsym                               { varsym_loose_infix }
}

-- ToDo: - move `var` and (sym) into lexical syntax?
--       - remove backquote from $special?
<0> {
  @qvarsym                                         { idtoken qvarsym }
  @qconsym                                         { idtoken qconsym }
  @consym                                          { consym }
}

-- For the normal boxed literals we need to be careful
-- when trying to be close to Haskell98

<0> {
  -- Normal integral literals (:: Num a => a, from Integer)
  @decimal                                                                   { tok_num positive 0 0 decimal }
  0[oO] @octal                                                       { tok_num positive 2 2 octal }
  0[xX] @hexadecimal                                                 { tok_num positive 2 2 hexadecimal }
  @negative @decimal                   / { negLitPred }                      { tok_num negative 1 1 decimal }
  @negative 0[oO] @octal       / { negLitPred }                      { tok_num negative 3 3 octal }
  @negative 0[xX] @hexadecimal / { negLitPred }                      { tok_num negative 3 3 hexadecimal }

  -- Normal rational literals (:: Fractional a => a, from Rational)
  @floating_point                                                            { tok_frac 0 tok_float }
  @negative @floating_point            / { negLitPred }                      { tok_frac 0 tok_float }
}

-- Strings and chars are lexed by hand-written code.  The reason is
-- that even if we recognise the string or char here in the regex
-- lexer, we would still have to parse the string afterward in order
-- to convert it to a String.
<0> {
  \'                            { lex_char_tok }
  \"                            { lex_string_tok }
}

-- Note [Whitespace-sensitive operator parsing]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In accord with GHC Proposal #229 https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst
-- we classify operator occurrences into four categories:
--
--     a ! b   -- a loose infix occurrence
--     a!b     -- a tight infix occurrence
--     a !b    -- a prefix occurrence
--     a! b    -- a suffix occurrence
--
-- The rules are a bit more elaborate than simply checking for whitespace, in
-- order to accommodate the following use cases:
--
--     f (!a) = ...    -- prefix occurrence
--     g (a !)         -- loose infix occurrence
--     g (! a)         -- loose infix occurrence
--
-- The precise rules are as follows:
--
--  * Identifiers, literals, and opening brackets (, (#, (|, [, [|, [||, [p|,
--    [e|, [t|, {, ⟦, ⦇, are considered "opening tokens". The function
--    followedByOpeningToken tests whether the next token is an opening token.
--
--  * Identifiers, literals, and closing brackets ), #), |), ], |], }, ⟧, ⦈,
--    are considered "closing tokens". The function precededByClosingToken tests
--    whether the previous token is a closing token.
--
--  * Whitespace, comments, separators, and other tokens, are considered
--    neither opening nor closing.
--
--  * Any unqualified operator occurrence is classified as prefix, suffix, or
--    tight/loose infix, based on preceding and following tokens:
--
--       precededByClosingToken | followedByOpeningToken | Occurrence
--      ------------------------+------------------------+------------
--       False                  | True                   | prefix
--       True                   | False                  | suffix
--       True                   | True                   | tight infix
--       False                  | False                  | loose infix
--      ------------------------+------------------------+------------
--
-- A loose infix occurrence is always considered an operator. Other types of
-- occurrences may be assigned a special per-operator meaning override:
--
--   Operator |  Occurrence   | Token returned
--  ----------+---------------+------------------------------------------
--    !       |  prefix       | ITbang
--            |               |   strictness annotation or bang pattern,
--            |               |   e.g.  f !x = rhs, data T = MkT !a
--            |  not prefix   | ITvarsym "!"
--            |               |   ordinary operator or type operator,
--            |               |   e.g.  xs ! 3, (! x), Int ! Bool
--  ----------+---------------+------------------------------------------
--    ~       |  prefix       | ITtilde
--            |               |   laziness annotation or lazy pattern,
--            |               |   e.g.  f ~x = rhs, data T = MkT ~a
--            |  not prefix   | ITvarsym "~"
--            |               |   ordinary operator or type operator,
--            |               |   e.g.  xs ~ 3, (~ x), Int ~ Bool
--  ----------+---------------+------------------------------------------
--    $  $$   |  prefix       | ITdollar, ITdollardollar
--            |               |   untyped or typed Template Haskell splice,
--            |               |   e.g.  $(f x), $$(f x), $$"str"
--            |  not prefix   | ITvarsym "$", ITvarsym "$$"
--            |               |   ordinary operator or type operator,
--            |               |   e.g.  f $ g x, a $$ b
--  ----------+---------------+------------------------------------------
--    @       |  prefix       | ITtypeApp
--            |               |   type application, e.g.  fmap @Maybe
--            |  tight infix  | ITat
--            |               |   as-pattern, e.g.  f p@(a,b) = rhs
--            |  suffix       | parse error
--            |               |   e.g. f p@ x = rhs
--            |  loose infix  | ITvarsym "@"
--            |               |   ordinary operator or type operator,
--            |               |   e.g.  f @ g, (f @)
--  ----------+---------------+------------------------------------------
--
-- Also, some of these overrides are guarded behind language extensions.
-- According to the specification, we must determine the occurrence based on
-- surrounding *tokens* (see the proposal for the exact rules). However, in
-- the implementation we cheat a little and do the classification based on
-- characters, for reasons of both simplicity and efficiency (see
-- 'followedByOpeningToken' and 'precededByClosingToken')
--
-- When an operator is subject to a meaning override, it is mapped to special
-- token: ITbang, ITtilde, ITat, ITdollar, ITdollardollar. Otherwise, it is
-- returned as ITvarsym.
--
-- For example, this is how we process the (!):
--
--    precededByClosingToken | followedByOpeningToken | Token
--   ------------------------+------------------------+-------------
--    False                  | True                   | ITbang
--    True                   | False                  | ITvarsym "!"
--    True                   | True                   | ITvarsym "!"
--    False                  | False                  | ITvarsym "!"
--   ------------------------+------------------------+-------------
--
-- And this is how we process the (@):
--
--    precededByClosingToken | followedByOpeningToken | Token
--   ------------------------+------------------------+-------------
--    False                  | True                   | ITtypeApp
--    True                   | False                  | parse error
--    True                   | True                   | ITat
--    False                  | False                  | ITvarsym "@"
--   ------------------------+------------------------+-------------

-- -----------------------------------------------------------------------------
-- Alex "Haskell code fragment bottom"

{

type AnnKeywordId = ()
data LexErrKind = LexErrKind_UTF8 | LexErrKind_Char Char | LexErrKind_EOF
type StringBuffer = (Char, String)

-- | Check whether a 'StringBuffer' is empty (analogous to 'Data.List.null').
atEnd :: StringBuffer -> Bool
atEnd (_, str) = null str

data LexErr = LexError | LexStringCharLitEOF | LexStringCharLit | LexNumEscapeRange
type GenSemic = Bool
type Bag a = [a]
consBag :: a -> Bag a -> Bag a
consBag = (:)
type ApiAnnKey = (RealSrcSpan,AnnKeywordId)
type ExtsBitmap = ()
data Warning = Empty

data ErrorDesc = ErrLexer LexErr LexErrKind | ErrParse String | ErrMissingBlock | ErrSuffixAT

data Error = Error
   { errDesc  :: !ErrorDesc   -- ^ Error description
   , errLoc   :: !RealSrcSpan     -- ^ Error position
   }

nextChar :: StringBuffer -> (Char,StringBuffer)
nextChar (_, (c:x:xs)) = (c, (x, xs))
nextChar _ = error "nextChar: empty StringBuffer"

currentChar :: StringBuffer -> Char
currentChar (_, (x:_)) = x

lexemeToString :: StringBuffer -> Int -> String
lexemeToString (_, s) n = take n s

lexemeToFastString :: StringBuffer -> Int -> FastString
lexemeToFastString sb n = fsLit $ lexemeToString sb n

byteDiff :: StringBuffer -> StringBuffer -> Int
byteDiff (_, l1) (_, l2) = length l2 - length l1

offsetBytes :: Int -> StringBuffer -> StringBuffer
offsetBytes 0 s = s
offsetBytes n (_, x:xs) = offsetBytes (n-1) (x, xs)

prevChar :: StringBuffer -> Char -> Char
prevChar (x, _) _ = x

stepOn :: StringBuffer -> StringBuffer
stepOn (_, (x:xs)) = (x, xs)


emptyBag :: [a]
emptyBag = []

panic :: String -> a
panic str = error $ "panic: " <> str

data FractionalLit
  = FL { fl_text :: String     -- How the value was written in the source
       , fl_neg :: Bool            -- See Note [Negative zero]
       , fl_value :: Rational      -- Numeric value of the literal
       }
  deriving (Show)

data IntegralLit
  = IL { il_text :: String
       , il_neg :: Bool -- See Note [Negative zero]
       , il_value :: Integer
       }
  deriving (Show)

-- -----------------------------------------------------------------------------
-- The token type

data Token
  = ITas                        -- Haskell keywords
  | ITcase
  | ITclass
  | ITdata
  | ITdefault
  | ITderiving
  | ITdo (Maybe FastString)
  | ITelse
  | IThiding
  | ITforeign
  | ITif
  | ITimport
  | ITin
  | ITinfix
  | ITinfixl
  | ITinfixr
  | ITinstance
  | ITlet
  | ITmodule
  | ITnewtype
  | ITof
  | ITqualified
  | ITthen
  | ITtype
  | ITwhere

  | ITforall
  | ITexport
  | ITlabel
  | ITdynamic
  | ITsafe
  | ITinterruptible
  | ITunsafe
  | ITstdcallconv
  | ITccallconv
  | ITcapiconv
  | ITmdo (Maybe FastString)
  | ITfamily
  | ITrole
  | ITgroup
  | ITby
  | ITusing
  | ITpattern
  | ITstatic
  | ITstock
  | ITanyclass
  | ITvia

  | ITdotdot                    -- reserved symbols
  | ITcolon
  | ITdcolon
  | ITequal
  | ITlam
  | ITvbar
  | ITlarrow
  | ITrarrow
  | ITdarrow
  | ITprefixminus -- See Note [Minus tokens]
  | ITbang     -- Prefix (!) only, e.g. f !x = rhs
  | ITtilde    -- Prefix (~) only, e.g. f ~x = rhs
  | ITat       -- Tight infix (@) only, e.g. f x@pat = rhs
  | ITtypeApp  -- Prefix (@) only, e.g. f @t
  | ITstar
  | ITdot

  | ITocurly                    -- special symbols
  | ITccurly
  | ITvocurly
  | ITvccurly
  | ITobrack
  | ITcbrack
  | IToparen
  | ITcparen
  | IToubxparen
  | ITcubxparen
  | ITsemi
  | ITcomma
  | ITunderscore
  | ITbackquote
  | ITsimpleQuote               --  '

  | ITvarid   FastString        -- identifiers
  | ITvarsym  FastString
  | ITconsym  FastString
  | ITqvarid  (FastString,FastString)
  | ITqvarsym (FastString,FastString)
  | ITqconsym (FastString,FastString)

  | ITchar     String Char       -- Note [Literal source text] in "GHC.Types.Basic"
  | ITstring   String FastString -- Note [Literal source text] in "GHC.Types.Basic"
  | ITinteger  IntegralLit           -- Note [Literal source text] in "GHC.Types.Basic"
  | ITrational FractionalLit

  | ITunknown String             -- ^ Used when the lexer can't make sense of it
  | ITeof                        -- ^ end of file token

  -- Documentation annotations
  | ITlineComment     String     -- ^ comment starting by "--"

  deriving Show

reservedWordsFM :: Map FastString Token
reservedWordsFM = Map.fromList $
    map (\(x, y, _) -> (mkFastString x, y))
        [( "_",              ITunderscore,    0 ),
         ( "as",             ITas,            0 ),
         ( "case",           ITcase,          0 ),
         ( "class",          ITclass,         0 ),
         ( "data",           ITdata,          0 ),
         ( "default",        ITdefault,       0 ),
         ( "deriving",       ITderiving,      0 ),
         ( "do",             ITdo Nothing,    0 ),
         ( "else",           ITelse,          0 ),
         ( "hiding",         IThiding,        0 ),
         ( "if",             ITif,            0 ),
         ( "import",         ITimport,        0 ),
         ( "in",             ITin,            0 ),
         ( "infix",          ITinfix,         0 ),
         ( "infixl",         ITinfixl,        0 ),
         ( "infixr",         ITinfixr,        0 ),
         ( "instance",       ITinstance,      0 ),
         ( "let",            ITlet,           0 ),
         ( "module",         ITmodule,        0 ),
         ( "newtype",        ITnewtype,       0 ),
         ( "of",             ITof,            0 ),
         ( "qualified",      ITqualified,     0 ),
         ( "then",           ITthen,          0 ),
         ( "type",           ITtype,          0 ),
         ( "mdo",            ITmdo Nothing,   0 ),
             -- See Note [Lexing type pseudo-keywords]
         ( "family",         ITfamily,        0 ),
         ( "role",           ITrole,          0 ),
         ( "pattern",        ITpattern,       0 ),
         ( "static",         ITstatic,        0 ),
         ( "stock",          ITstock,         0 ),
         ( "anyclass",       ITanyclass,      0 ),
         ( "via",            ITvia,           0 ),
         ( "group",          ITgroup,         0 ),
         ( "by",             ITby,            0 ),
         ( "using",          ITusing,         0 ),

         ( "foreign",        ITforeign,       0 ),
         ( "export",         ITexport,        0 ),
         ( "label",          ITlabel,         0 ),
         ( "dynamic",        ITdynamic,       0 ),
         ( "safe",           ITsafe,          0 ),
         ( "interruptible",  ITinterruptible, 0 ),
         ( "unsafe",         ITunsafe,        0 ),
         ( "stdcall",        ITstdcallconv,   0 ),
         ( "ccall",          ITccallconv,     0 ),
         ( "capi",           ITcapiconv,      0 )
     ]

{-----------------------------------
Note [Lexing type pseudo-keywords]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One might think that we wish to treat 'family' and 'role' as regular old
varids whenever -XTypeFamilies and -XRoleAnnotations are off, respectively.
But, there is no need to do so. These pseudo-keywords are not stolen syntax:
on. In fact, by unconditionally lexing these pseudo-keywords as special, we
can get better error messages.

Also, note that these are included in the `varid` production in the parser --
a key detail to make all this work.
-------------------------------------}

reservedSymsFM :: Map FastString Token
reservedSymsFM = Map.fromList $
  map (\(x,w) -> (mkFastString x,w))
    [ ("..",  ITdotdot)
      ,(":",   ITcolon)
      ,("::",  ITdcolon)
      ,("=",   ITequal)
      ,("\\",  ITlam)
      ,("|",   ITvbar)
      ,("<-",  ITlarrow)
      ,("->",  ITrarrow)
      ,("=>",  ITdarrow)
      ,("*",   ITstar)
      ,(".",   ITdot)
      ,("∷",   ITdcolon)
      ,("⇒",   ITdarrow)
      ,("∀",   ITforall)
      ,("→",   ITrarrow)
      ,("←",   ITlarrow)
      ,("★",   ITstar)
      ]

-- -----------------------------------------------------------------------------
-- Lexer actions

type Action = RealSrcSpan -> StringBuffer -> Int -> P (SrcLocated Token)

special :: Token -> Action
special tok span _buf _len = return (L span tok)

token, layout_token :: Token -> Action
token t span _buf _len = return (L span t)
layout_token t span _buf _len = pushLexState layout >> return (L span t)

idtoken :: (StringBuffer -> Int -> Token) -> Action
idtoken f span buf len = return (L span $! (f buf len))

qdo_token :: (Maybe FastString -> Token) -> Action
qdo_token con span buf len = do
    maybe_layout token
    return (L span $! token)
  where
    !token = con $! Just $! fst $! splitQualName buf len False

begin :: Int -> Action
begin code _span _str _len = do pushLexState code; lexToken

pop :: Action
pop _span _buf _len = popLexState *> lexToken

hopefully_open_brace :: Action
hopefully_open_brace span buf len
 = do ctx <- getContext
      (AI l _) <- getInput
      let offset = srcLocCol l
          isOK = case ctx of
                 Layout prev_off _ : _ -> prev_off < offset
                 _                     -> True
      if isOK then pop_and open_brace span buf len
              else addFatalError $ Error ErrMissingBlock span

pop_and :: Action -> Action
pop_and act span buf len = do _ <- popLexState
                              act span buf len

-- See Note [Whitespace-sensitive operator parsing]
followedByOpeningToken :: AlexAccPred ExtsBitmap
followedByOpeningToken _ _ _ (AI _ buf)
  | atEnd buf = False
  | otherwise =
      case nextChar buf of
        ('{', buf') -> nextCharIsNot buf' (== '-')
        ('(', _) -> True
        ('[', _) -> True
        ('\"', _) -> True
        ('\'', _) -> True
        ('_', _) -> True
        ('⟦', _) -> True
        ('⦇', _) -> True
        (c, _) -> isAlphaNum c

-- See Note [Whitespace-sensitive operator parsing]
precededByClosingToken :: AlexAccPred ExtsBitmap
precededByClosingToken _ (AI _ buf) _ _ =
  case prevChar buf '\n' of
    '}' -> True
    ')' -> True
    ']' -> True
    '\"' -> True
    '\'' -> True
    '_' -> True
    '⟧' -> True
    '⦈' -> True
    c -> isAlphaNum c

{-# INLINE nextCharIs #-}
nextCharIs :: StringBuffer -> (Char -> Bool) -> Bool
nextCharIs buf p = not (atEnd buf) && p (currentChar buf)

{-# INLINE nextCharIsNot #-}
nextCharIsNot :: StringBuffer -> (Char -> Bool) -> Bool
nextCharIsNot buf p = not (nextCharIs buf p)

notFollowedBy :: Char -> AlexAccPred ExtsBitmap
notFollowedBy char _ _ _ (AI _ buf)
  = nextCharIsNot buf (== char)

notFollowedBySymbol :: AlexAccPred ExtsBitmap
notFollowedBySymbol _ _ _ (AI _ buf)
  = nextCharIsNot buf (`elem` ("!#$%&*+./<=>?@\\^|-~" :: String))

-- Check if we should parse a negative literal (e.g. -123) as a single token.
negLitPred :: AlexAccPred ExtsBitmap
negLitPred = alexNotPred precededByClosingToken

alexNotPred p userState in1 len in2
  = not (p userState in1 len in2)

lineCommentToken :: Action
lineCommentToken span buf len = lexToken

open_brace, close_brace :: Action
open_brace span _str _len = do
  ctx <- getContext
  setContext (NoLayout:ctx)
  return (L span ITocurly)
close_brace span _str _len = do
  popContext
  return (L span ITccurly)

qvarid :: StringBuffer -> Int -> Token
qvarid buf len = ITqvarid $! splitQualName buf len False

splitQualName :: StringBuffer -> Int -> Bool -> (FastString,FastString)
-- takes a StringBuffer and a length, and returns the module name
-- and identifier parts of a qualified name.  Splits at the *last* dot,
-- because of hierarchical module names.
--
-- Throws an error if the name is not qualified.
splitQualName orig_buf len parens = split orig_buf orig_buf
  where
    split buf dot_buf
        | orig_buf `byteDiff` buf >= len  = done dot_buf
        | c == '.'                        = found_dot buf'
        | otherwise                       = split buf' dot_buf
      where
       (c,buf') = nextChar buf

    -- careful, we might get names like M....
    -- so, if the character after the dot is not upper-case, this is
    -- the end of the qualifier part.
    found_dot buf -- buf points after the '.'
        | isUpper c    = split buf' buf
        | otherwise    = done buf
      where
       (c,buf') = nextChar buf

    done dot_buf
        | qual_size < 1 = error "splitQualName got an unqualified named"
        | otherwise =
        (lexemeToFastString orig_buf (qual_size - 1),
         if parens -- Prelude.(+)
            then lexemeToFastString (stepOn dot_buf) (len - qual_size - 2)
            else lexemeToFastString dot_buf (len - qual_size))
      where
        qual_size = orig_buf `byteDiff` dot_buf

varid :: Action
varid span buf len =
  case Map.lookup fs reservedWordsFM of
    Just keyword -> do
      maybe_layout keyword
      return $ L span keyword
    Nothing ->
      return $ L span $ ITvarid fs
  where
    !fs = error $ show (buf, len) -- lexemeToFastString buf len

qvarsym, qconsym :: StringBuffer -> Int -> Token
qvarsym buf len = ITqvarsym $! splitQualName buf len False
qconsym buf len = ITqconsym $! splitQualName buf len False

-- See Note [Whitespace-sensitive operator parsing]
varsym_prefix :: Action
varsym_prefix = sym $ \s ->
  if | s == fsLit "@"  -- regardless of TypeApplications for better error messages
     -> return ITtypeApp
     | s == fsLit "-"
     -> return ITprefixminus
     | s == fsLit "!" -> return ITbang
     | s == fsLit "~" -> return ITtilde
     | otherwise -> return (ITvarsym s)

-- See Note [Whitespace-sensitive operator parsing]
varsym_suffix :: Action
varsym_suffix = sym $ \s ->
  if | s == fsLit "@" -> failMsgP (Error ErrSuffixAT)
     | otherwise      -> return (ITvarsym s)

-- See Note [Whitespace-sensitive operator parsing]
varsym_tight_infix :: Action
varsym_tight_infix = sym $ \s ->
  if | s == fsLit "@" -> return ITat
     | otherwise -> return (ITvarsym s)

-- See Note [Whitespace-sensitive operator parsing]
varsym_loose_infix :: Action
varsym_loose_infix = sym (\s -> return $ ITvarsym s)

consym :: Action
consym = sym (\s -> return $ ITconsym s)

sym :: (FastString -> P Token) -> Action
sym con span buf len =
  case Map.lookup fs reservedSymsFM of
    Just keyword ->
      return $ L span keyword
    Nothing -> do
      L span <$!> con fs
  where
    !fs = lexemeToFastString buf len

-- Variations on the integral numeric literal.
tok_integral :: (String -> Integer -> Token)
             -> (Integer -> Integer)
             -> Int -> Int
             -> (Integer, (Char -> Int))
             -> Action
tok_integral itint transint transbuf translen (radix,char_to_int) span buf len = do
  let src = lexemeToString buf len
  return $ L span $ itint src
       $! transint $ parseUnsignedInteger
       (offsetBytes transbuf buf) (subtract translen len) radix char_to_int

tok_num :: (Integer -> Integer)
        -> Int -> Int
        -> (Integer, (Char->Int)) -> Action
tok_num = tok_integral $ \case
    st@('-':_) -> itint st (const True)
    st@_       -> itint st (const False)
  where
    itint :: String -> (Integer -> Bool) -> Integer -> Token
    itint !st is_negative !val = ITinteger ((IL st $! is_negative val) val)

positive, negative :: (Integer -> Integer)
positive = id
negative = negate
decimal, octal, hexadecimal :: (Integer, Char -> Int)
decimal = (10,octDecDigit)
binary = (2,octDecDigit)
octal = (8,octDecDigit)
hexadecimal = (16,hexDigit)

-- readRational can understand negative rationals, exponents, everything.
tok_frac :: Int -> (String -> Token) -> Action
tok_frac drop f span buf len = do
  let src = lexemeToString buf (len-drop)
  return (L span $! (f $! src))

tok_float :: String -> Token
tok_float        str = ITrational   $! readFractionalLit str
tok_hex_float    str = ITrational   $! readHexFractionalLit str

readFractionalLit :: String -> FractionalLit
readFractionalLit = undefined

readHexFractionalLit = error "readHexFractionalLit"

hexDigit :: Char -> Int
hexDigit c | is_decdigit c = ord c - ord '0'
           | otherwise     = ord (to_lower c) - ord 'a' + 10

is_decdigit :: Char -> Bool
is_decdigit = error "is_decdigit"

octDecDigit :: Char -> Int
octDecDigit = error "octDecDigit"

parseUnsignedInteger = undefined

to_lower :: Char -> Char
to_lower c
  | c >=  'A' && c <= 'Z' = chr (ord c - (ord 'A' - ord 'a'))
  | otherwise = c

-- -----------------------------------------------------------------------------
-- Layout processing

-- we're at the first token on a line, insert layout tokens if necessary
do_bol :: Action
do_bol span _str _len = do
  (pos, gen_semic) <- getOffside
  case pos of
      LT -> do
          --trace "layout: inserting '}'" $ do
          popContext
          -- do NOT pop the lex state, we might have a ';' to insert
          return (L span ITvccurly)
      EQ | gen_semic -> do
          --trace "layout: inserting ';'" $ do
          _ <- popLexState
          return (L span ITsemi)
      _ -> do
          _ <- popLexState
          lexToken

maybe_layout ITlet       = pushLexState layout
maybe_layout ITwhere     = pushLexState layout
maybe_layout ITif        = pushLexState layout_if
maybe_layout _           = return ()

-- Pushing a new implicit layout context.  If the indentation of the
-- next token is not greater than the previous layout context, then
-- Haskell 98 says that the new layout context should be empty; that is
-- the lexer must generate {}.
--
-- We are slightly more lenient than this: when the new context is started
-- by a 'do', then we allow the new context to be at the same indentation as
-- the previous context.  This is what the 'strict' argument is for.
new_layout_context :: Bool -> Bool -> Token -> Action
new_layout_context strict gen_semic tok span _buf len = do
    _ <- popLexState
    (AI l _) <- getInput
    let offset = srcLocCol l - len
    ctx <- getContext
    case ctx of
        Layout prev_off _ : _  |
           (strict     && prev_off >= offset  ||
            not strict && prev_off > offset) -> do
                -- token is indented to the left of the previous context.
                -- we must generate a {} sequence now.
                pushLexState layout_left
                return (L span tok)
        _ -> do setContext (Layout offset gen_semic : ctx)
                return (L span tok)

do_layout_left :: Action
do_layout_left span _buf _len = do
    _ <- popLexState
    pushLexState bol  -- we must be at the start of a line
    return (L span ITvccurly)

-- -----------------------------------------------------------------------------
-- Strings & Chars

-- This stuff is horrible.  I hates it.

lex_string_tok :: Action
lex_string_tok span buf _len = do
  tok <- lex_string "" ""
  (AI end bufEnd) <- getInput
  return (L (mkRealSrcSpan (realSrcSpanStart span) end) tok)

lex_string :: String -> String -> P Token
lex_string src s = do
  i <- getInput
  case alexGetChar' i of
    Nothing -> lit_error i

    Just ('"',i)  -> do
        setInput i
        let s' = reverse s
        return (ITstring (reverse src) (fsLit s'))
    
    Just (c, i) -> lex_string (c:src) (c:s)

lex_char_tok :: Action
lex_char_tok = undefined

readHexRational = undefined

readRational = undefined

readNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Char
readNum is_digit base conv = do
  i <- getInput
  c <- getCharOrFail i
  if is_digit c
        then readNum2 is_digit base conv (conv c)
        else lit_error i

readNum2 :: (Char -> Bool) -> Int -> (Char -> Int) -> Int -> P Char
readNum2 is_digit base conv i = do
  input <- getInput
  read i input
  where read i input = do
          case alexGetChar' input of
            Just (c,input') | is_digit c -> do
               let i' = i*base + conv c
               if i' > 0x10ffff
                  then setInput input >> lexError LexNumEscapeRange
                  else read i' input'
            _other -> do
              setInput input; return (chr i)


silly_escape_chars :: [(String, Char)]
silly_escape_chars = [
        ("NUL", '\NUL'),
        ("SOH", '\SOH'),
        ("STX", '\STX'),
        ("ETX", '\ETX'),
        ("EOT", '\EOT'),
        ("ENQ", '\ENQ'),
        ("ACK", '\ACK'),
        ("BEL", '\BEL'),
        ("BS", '\BS'),
        ("HT", '\HT'),
        ("LF", '\LF'),
        ("VT", '\VT'),
        ("FF", '\FF'),
        ("CR", '\CR'),
        ("SO", '\SO'),
        ("SI", '\SI'),
        ("DLE", '\DLE'),
        ("DC1", '\DC1'),
        ("DC2", '\DC2'),
        ("DC3", '\DC3'),
        ("DC4", '\DC4'),
        ("NAK", '\NAK'),
        ("SYN", '\SYN'),
        ("ETB", '\ETB'),
        ("CAN", '\CAN'),
        ("EM", '\EM'),
        ("SUB", '\SUB'),
        ("ESC", '\ESC'),
        ("FS", '\FS'),
        ("GS", '\GS'),
        ("RS", '\RS'),
        ("US", '\US'),
        ("SP", '\SP'),
        ("DEL", '\DEL')
        ]

-- before calling lit_error, ensure that the current input is pointing to
-- the position of the error in the buffer.  This is so that we can report
-- a correct location to the user, but also so we can detect UTF-8 decoding
-- errors if they occur.
lit_error :: AlexInput -> P a
lit_error i = do setInput i; lexError LexStringCharLit

getCharOrFail :: AlexInput -> P Char
getCharOrFail i =  do
  case alexGetChar' i of
        Nothing -> lexError LexStringCharLitEOF
        Just (c,i)  -> do setInput i; return c

-- -----------------------------------------------------------------------------

warnTab :: Action
warnTab srcspan _buf _len = lexToken -- TODO: remove

-- -----------------------------------------------------------------------------
-- The Parse Monad

-- | Do we want to generate ';' layout tokens? In some cases we just want to
-- generate '}', e.g. in MultiWayIf we don't need ';'s because '|' separates

generateSemic, dontGenerateSemic :: GenSemic
generateSemic     = True
dontGenerateSemic = False

data LayoutContext
  = NoLayout
  | Layout !Int !GenSemic
  deriving Show

-- | The result of running a parser.
data ParseResult a
  = POk      -- ^ The parser has consumed a (possibly empty) prefix
             --   of the input and produced a result.
      PState -- ^ The resulting parsing state. Can be used to resume parsing.
      a      -- ^ The resulting value.
  | PFailed  -- ^ The parser has consumed a (possibly empty) prefix
             --   of the input and failed.
      PState -- ^ The parsing state right before failure, including the fatal
             --   parse error. 'getErrorMessages' must return
             --   a non-empty bag of errors.

data PState = PState {
        buffer     :: StringBuffer,
        warnings   :: Bag Warning,
        errors     :: Bag Error,
        last_tk    :: Maybe Token,
        last_loc   :: RealSrcSpan,      -- pos of previous token
        last_len   :: !Int,        -- len of previous token
        loc        :: RealSrcLoc,       -- current loc (end of prev token + 1)
        context    :: [LayoutContext],
        lex_state  :: [Int],
        srcfiles   :: [FastString],

        -- The next three are used to implement Annotations giving the
        -- locations of 'noise' tokens in the source, so that users of
        -- the GHC API can do source to source conversions.
        -- See note [Api annotations] in GHC.Parser.Annotation
        annotations :: [(ApiAnnKey,[RealSrcSpan])],
        eof_pos :: Maybe RealSrcSpan,
        comment_q :: [Located AnnotationComment],
        annotations_comments :: [(RealSrcSpan,[Located AnnotationComment])]
     }
        -- last_loc and last_len are used when generating error messages,
        -- and in pushCurrentContext only.  Sigh, if only Happy passed the
        -- current token to happyError, we could at least get rid of last_len.
        -- Getting rid of last_loc would require finding another way to
        -- implement pushCurrentContext (which is only called from one place).

-- | The parsing monad, isomorphic to @StateT PState Maybe@.
newtype P a = P { unP :: PState -> ParseResult a }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure = returnP
  (<*>) = ap

instance Monad P where
  (>>=) = thenP

returnP :: a -> P a
returnP a = a `seq` (P $ \s -> POk s a)

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \ s ->
        case m s of
                POk s1 a         -> (unP (k a)) s1
                PFailed s1 -> PFailed s1

failMsgP :: (RealSrcSpan -> Error) -> P a
failMsgP f = do
  pState <- getPState
  addFatalError (f (last_loc pState))

failLocMsgP :: RealSrcLoc -> RealSrcLoc -> (RealSrcSpan -> Error) -> P a
failLocMsgP loc1 loc2 f =
  addFatalError (f (mkRealSrcSpan loc1 loc2))

getPState :: P PState
getPState = P $ \s -> POk s s

getRealSrcLoc :: P RealSrcLoc
getRealSrcLoc = P $ \s@(PState{ loc=loc }) -> POk s loc

addSrcFile :: FastString -> P ()
addSrcFile f = P $ \s -> POk s{ srcfiles = f : srcfiles s } ()

setEofPos :: RealSrcSpan -> P ()
setEofPos span = P $ \s -> POk s{ eof_pos = Just span } ()

setLastToken :: RealSrcSpan -> Int -> P ()
setLastToken loc len = P $ \s -> POk s {
  last_loc=loc,
  last_len=len
  } ()

setLastTk :: Token -> P ()
setLastTk tk = P $ \s -> POk s { last_tk = Just tk } ()

getLastTk :: P (Maybe Token)
getLastTk = P $ \s@(PState { last_tk = last_tk }) -> POk s last_tk

data AlexInput = AI !RealSrcLoc !StringBuffer

{-
Note [Unicode in Alex]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Although newer versions of Alex support unicode, this grammar is processed with
the old style '--latin1' behaviour. This means that when implementing the
functions

    alexGetByte       :: AlexInput -> Maybe (Word8,AlexInput)
    alexInputPrevChar :: AlexInput -> Char

which Alex uses to take apart our 'AlexInput', we must

  * return a latin1 character in the 'Word8' that 'alexGetByte' expects
  * return a latin1 character in 'alexInputPrevChar'.

We handle this in 'adjustChar' by squishing entire classes of unicode
characters into single bytes.
-}

{-# INLINE adjustChar #-}
adjustChar :: Char -> Word8
adjustChar c = fromIntegral $ ord adj_c
  where non_graphic     = '\x00'
        upper           = '\x01'
        lower           = '\x02'
        digit           = '\x03'
        symbol          = '\x04'
        space           = '\x05'
        other_graphic   = '\x06'
        uniidchar       = '\x07'

        adj_c
          | c <= '\x07' = non_graphic
          | c <= '\x7f' = c
          -- Alex doesn't handle Unicode, so when Unicode
          -- character is encountered we output these values
          -- with the actual character value hidden in the state.
          | otherwise =
                -- NB: The logic behind these definitions is also reflected
                -- in "GHC.Utils.Lexeme"
                -- Any changes here should likely be reflected there.

                case generalCategory c of
                  UppercaseLetter       -> upper
                  LowercaseLetter       -> lower
                  TitlecaseLetter       -> upper
                  ModifierLetter        -> uniidchar -- see #10196
                  OtherLetter           -> lower -- see #1103
                  NonSpacingMark        -> uniidchar -- see #7650
                  SpacingCombiningMark  -> other_graphic
                  EnclosingMark         -> other_graphic
                  DecimalNumber         -> digit
                  LetterNumber          -> other_graphic
                  OtherNumber           -> digit -- see #4373
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OpenPunctuation       -> other_graphic
                  ClosePunctuation      -> other_graphic
                  InitialQuote          -> other_graphic
                  FinalQuote            -> other_graphic
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  _other                -> non_graphic

-- Getting the previous 'Char' isn't enough here - we need to convert it into
-- the same format that 'alexGetByte' would have produced.
--
-- See Note [Unicode in Alex] and #13986.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ buf) = chr (fromIntegral (adjustChar pc))
  where pc = prevChar buf '\n'

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar inp = case alexGetByte inp of
                    Nothing    -> Nothing
                    Just (b,i) -> c `seq` Just (c,i)
                     where c = chr $ fromIntegral b

-- See Note [Unicode in Alex]
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AI loc s)
  | atEnd s   = Nothing
  | otherwise = byte `seq` Just (byte, AI loc' s')
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        byte   = adjustChar c

-- This version does not squash unicode characters, it is used when
-- lexing strings.
alexGetChar' :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar' (AI loc s)
  | atEnd s   = Nothing
  | otherwise = c `seq` Just (c, AI loc' s')
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c

getInput :: P AlexInput
getInput = P $ \s@PState{ loc=l, buffer=b } -> POk s (AI l b)

setInput :: AlexInput -> P ()
setInput (AI l b) = P $ \s -> POk s{ loc=l, buffer=b } ()

nextIsEOF :: P Bool
nextIsEOF = do
  AI _ s <- getInput
  return $ atEnd s

pushLexState :: Int -> P ()
pushLexState ls = P $ \s@PState{ lex_state=l } -> POk s{lex_state=ls:l} ()

popLexState :: P Int
popLexState = P $ \s@PState{ lex_state=ls:l } -> POk s{ lex_state=l } ls

getLexState :: P Int
getLexState = P $ \s@PState{ lex_state=ls:_ } -> POk s ls

initParserState :: StringBuffer -> RealSrcLoc -> PState
initParserState buf loc' =
  PState {
      buffer        = buf,
      errors        = emptyBag,
      warnings      = emptyBag,
      last_tk       = Nothing,
      last_loc      = mkRealSrcSpan loc' loc',
      last_len      = 0,
      loc           = loc',
      context       = [],
      lex_state     = [bol, 0],
      srcfiles      = [],
      annotations = [],
      eof_pos = Nothing,
      comment_q = [],
      annotations_comments = []
    }

-- | Add a non-fatal error. Use this when the parser can produce a result
--   despite the error.
--
--
--   For example, when GHC encounters a @forall@ in a type,
--   but @-XExplicitForAll@ is disabled, the parser constructs @ForAllTy@
--   as if @-XExplicitForAll@ was enabled, adding a non-fatal error to
--   the accumulator.
--
--   Control flow wise, non-fatal errors act like warnings: they are added
--   to the accumulator and parsing continues. This allows GHC to report
--   more than one parse error per file.
--
addError :: Error -> P ()

-- | Add a fatal error. This will be the last error reported by the parser, and
--   the parser will not produce any result, ending in a 'PFailed' state.
addFatalError :: Error -> P a

-- | Given a location and a list of AddAnn, apply them all to the location.
addAnnotation :: RealSrcSpan          -- RealSrcSpan of enclosing AST construct
              -> AnnKeywordId     -- The first two parameters are the key
              -> RealSrcSpan          -- The location of the keyword itself
              -> P ()

addError err
  = P $ \s -> POk s { errors = err `consBag` errors s} ()

addFatalError err =
  addError err >> P PFailed

addAnnotation l a v = do
  addAnnotationOnly l a v
  allocateCommentsP l
addAnnotation _ _ _ = return ()

-- | Get a bag of the errors that have been accumulated so far.
--   Does not take -Werror into account.
getErrorMessages :: PState -> Bag Error
getErrorMessages p = errors p

getContext :: P [LayoutContext]
getContext = P $ \s@PState{context=ctx} -> POk s ctx

setContext :: [LayoutContext] -> P ()
setContext ctx = P $ \s -> POk s{context=ctx} ()

popContext :: P ()
popContext = P $ \ s@(PState{ buffer = buf, context = ctx,
                              last_len = len, last_loc = last_loc }) ->
  case ctx of
        (_:tl) ->
          POk s{ context = tl } ()
        []     ->
          unP (addFatalError $ srcParseErr buf len last_loc) s

-- Push a new layout context at the indentation of the last token read.
pushCurrentContext :: GenSemic -> P ()
pushCurrentContext gen_semic = P $ \ s@PState{ last_loc=loc, context=ctx } ->
    POk s{context = Layout (srcSpanStartCol loc) gen_semic : ctx} ()

-- This is only used at the outer level of a module when the 'module' keyword is
-- missing.
pushModuleContext :: P ()
pushModuleContext = pushCurrentContext generateSemic

getOffside :: P (Ordering, Bool)
getOffside = P $ \s@PState{last_loc=loc, context=stk} ->
                let offs = srcSpanStartCol loc in
                let ord = case stk of
                            Layout n gen_semic : _ ->
                              --trace ("layout: " ++ show n ++ ", offs: " ++ show offs) $
                              (compare offs n, gen_semic)
                            _ ->
                              (GT, dontGenerateSemic)
                in POk s ord

-- ---------------------------------------------------------------------------
-- Construct a parse error

srcParseErr
  :: StringBuffer       -- current buffer (placed just after the last token)
  -> Int                -- length of the previous token
  -> RealSrcSpan
  -> Error
srcParseErr buf len loc = Error (ErrParse token) loc
  where
   token = lexemeToString (offsetBytes (-len) buf) len

-- Report a parse failure, giving the span of the previous token as
-- the location of the error.  This is the entry point for errors
-- detected during parsing.
srcParseFail :: P a
srcParseFail = P $ \s@PState{ buffer = buf, last_len = len,
                            last_loc = last_loc } ->
    unP (addFatalError $ srcParseErr buf len last_loc) s

-- A lexical error is reported at a particular position in the source file,
-- not over a token range.
lexError :: LexErr -> P a
lexError e = do
  loc <- getRealSrcLoc
  (AI end buf) <- getInput
  reportLexError loc end buf
    (\k -> Error (ErrLexer e k))

-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

lexer :: Bool -> (SrcLocated Token -> P a) -> P a

lexer queueComments cont = do
  (L span tok) <- lexToken
  --trace ("token: " ++ show tok) $ do
  if (queueComments && isComment tok)
    then queueComment (L span tok) >> lexer queueComments cont
    else cont (L span tok)

lexToken :: P (SrcLocated Token)
lexToken = do
  inp@(AI loc1 buf) <- getInput
  sc <- getLexState
  let exts = undefined -- getExts
  case alexScanUser exts inp sc of
    AlexEOF -> do
        let span = mkRealSrcSpan loc1 loc1
        setEofPos span
        setLastToken span 0
        return (L span ITeof)
    AlexError (AI loc2 buf) ->
        reportLexError loc1 loc2 buf
          (\k -> Error (ErrLexer LexError k))
    AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
    AlexToken inp2@(AI end buf2) _ t -> do
        setInput inp2
        let span = mkRealSrcSpan loc1 end
        let bytes = byteDiff buf buf2
        span `seq` setLastToken span bytes
        lt <- t span buf bytes
        let lt' = unLoc lt
        unless (isComment lt') (setLastTk lt')
        return lt

reportLexError :: RealSrcLoc -> RealSrcLoc -> StringBuffer -> (LexErrKind -> RealSrcSpan -> Error) -> P a
reportLexError loc1 loc2 buf f
  | atEnd buf = failLocMsgP loc1 loc2 (f LexErrKind_EOF)
  | otherwise =
  let c = fst (nextChar buf)
  in if c == '\0' -- decoding errors are mapped to '\0', see utf8DecodeChar#
     then failLocMsgP loc2 loc2 (f LexErrKind_UTF8)
     else failLocMsgP loc1 loc2 (f (LexErrKind_Char c))

lexTokenStream :: StringBuffer -> RealSrcLoc -> ParseResult [SrcLocated Token]
lexTokenStream buf loc = unP go initState
    where
    initState = initParserState buf loc
    go = do
      ltok <- lexer False return
      case ltok of
        L _ ITeof -> return []
        _ -> liftM (ltok:) go

{-
%************************************************************************
%*                                                                      *
        Helper functions for generating annotations in the parser
%*                                                                      *
%************************************************************************
-}


addAnnotationOnly :: RealSrcSpan -> AnnKeywordId -> RealSrcSpan -> P ()
addAnnotationOnly l a v = P $ \s -> POk s {
  annotations = ((l,a), [v]) : annotations s
  } ()


queueComment :: SrcLocated Token -> P()
queueComment c = P $ \s -> POk s {
  comment_q = commentToAnnotation c : comment_q s
  } ()

-- | Go through the @comment_q@ in @PState@ and remove all comments
-- that belong within the given span
allocateCommentsP :: RealSrcSpan -> P ()
allocateCommentsP ss = P $ \s ->
  let (comment_q', newAnns) = allocateComments ss (comment_q s) in
    POk s {
       comment_q = comment_q'
     , annotations_comments = newAnns ++ (annotations_comments s)
     } ()

allocateComments
  :: RealSrcSpan
  -> [Located AnnotationComment]
  -> ([Located AnnotationComment], [(RealSrcSpan,[Located AnnotationComment])])
allocateComments ss comment_q =
  let
    (before,rest)  = break (\(L l _) -> isRealSubspanOf l ss) comment_q
    (middle,after) = break (\(L l _) -> not (isRealSubspanOf l ss)) rest
    comment_q' = before ++ after
    newAnns = if null middle then []
                             else [(ss,middle)]
  in
    (comment_q', newAnns)

type AnnotationComment = String

commentToAnnotation :: Located Token -> Located AnnotationComment
commentToAnnotation (L l (ITlineComment s))     = L l s
commentToAnnotation _                           = panic "commentToAnnotation"

-- ---------------------------------------------------------------------

isComment :: Token -> Bool
isComment (ITlineComment     _)   = True
isComment _ = False

}
