{-# LANGUAGE DuplicateRecordFields #-}
module Vague.Parser.Located where

import Vague.FastString (FastString)

-- data Located a = L
--   { srcSpanFile :: !FastString
--   , srcSpanStart, srcSpanEnd :: {-# UNPACK #-} !Int
--   , unLoc :: a
--   }

-- | Real Source Span
data RealSrcSpan
  = RealSrcSpan'
        { srcSpanFile     :: !FastString,
          srcSpanSLine    :: {-# UNPACK #-} !Int,
          srcSpanSCol     :: {-# UNPACK #-} !Int,
          srcSpanELine    :: {-# UNPACK #-} !Int,
          srcSpanECol     :: {-# UNPACK #-} !Int
        }
  deriving (Eq, Ord, Show)

data RealSrcLoc = SrcLoc
  { srcLocFile :: FastString
  , srcLocLine :: {-# UNPACK #-} !Int     -- line number, begins at 1
  , srcLocCol :: {-# UNPACK #-} !Int     -- column number, begins at 1
  }
  deriving (Eq, Ord, Show)

-- | Create a 'SrcSpan' between two points in a file
mkRealSrcSpan :: RealSrcLoc -> RealSrcLoc -> RealSrcSpan
mkRealSrcSpan loc1 loc2 = RealSrcSpan' file line1 col1 line2 col2
  where
        line1 = srcLocLine loc1
        line2 = srcLocLine loc2
        col1 = srcLocCol loc1
        col2 = srcLocCol loc2
        file = srcLocFile loc1

data PsSpan
  = PsSpan { psRealSpan :: !RealSrcSpan, psBufSpan :: !BufSpan }
  deriving (Eq, Ord, Show)

type PsLocated = GenLocated PsSpan

data GenLocated l e = L l e
  deriving (Eq, Ord) -- , Functor, Foldable, Traversable)

data BufSpan =
  BufSpan { bufSpanStart, bufSpanEnd :: {-# UNPACK #-} !BufPos }
  deriving (Eq, Ord, Show)

newtype BufPos = BufPos { bufPos :: Int }
  deriving (Eq, Ord, Show)

data SrcSpan =
    RealSrcSpan !RealSrcSpan !(Maybe BufSpan)  -- See Note [Why Maybe BufPos]

data PsLoc
  = PsLoc { psRealLoc :: !RealSrcLoc, psBufPos :: !BufPos }
  deriving (Eq, Ord, Show)

type RealLocated = GenLocated RealSrcSpan

type Located = RealLocated

-- | Determines whether a span is enclosed by another one
isRealSubspanOf :: RealSrcSpan -- ^ The span that may be enclosed by the other
                -> RealSrcSpan -- ^ The span it may be enclosed by
                -> Bool
isRealSubspanOf src parent
    | srcSpanFile parent /= srcSpanFile src = False
    | otherwise = realSrcSpanStart parent <= realSrcSpanStart src &&
                  realSrcSpanEnd parent   >= realSrcSpanEnd src

realSrcSpanStart :: RealSrcSpan -> RealSrcLoc
realSrcSpanStart s = mkRealSrcLoc (srcSpanFile s)
                                  (srcSpanStartLine s)
                                  (srcSpanStartCol s)

realSrcSpanEnd :: RealSrcSpan -> RealSrcLoc
realSrcSpanEnd s = mkRealSrcLoc (srcSpanFile s)
                                (srcSpanEndLine s)
                                (srcSpanEndCol s)

srcSpanStartLine :: RealSrcSpan -> Int
srcSpanEndLine :: RealSrcSpan -> Int
srcSpanStartCol :: RealSrcSpan -> Int
srcSpanEndCol :: RealSrcSpan -> Int

srcSpanStartLine RealSrcSpan'{ srcSpanSLine=l } = l
srcSpanEndLine RealSrcSpan'{ srcSpanELine=l } = l
srcSpanStartCol RealSrcSpan'{ srcSpanSCol=l } = l
srcSpanEndCol RealSrcSpan'{ srcSpanECol=c } = c

mkRealSrcLoc :: FastString -> Int -> Int -> RealSrcLoc
mkRealSrcLoc x line col = SrcLoc x line col

mkPsSpan :: PsLoc -> PsLoc -> PsSpan
mkPsSpan (PsLoc r1 b1) (PsLoc r2 b2) = PsSpan (mkRealSrcSpan r1 r2) (BufSpan b1 b2)

mkSrcSpanPs :: PsSpan -> SrcSpan
mkSrcSpanPs (PsSpan r b) = RealSrcSpan r (Just b)

unLoc :: GenLocated l e -> e
unLoc (L _ e) = e

-- | Move the 'SrcLoc' down by one line if the character is a newline,
-- to the next 8-char tabstop if it is a tab, and across by one
-- character in any other case
advanceSrcLoc :: RealSrcLoc -> Char -> RealSrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f  (l + 1) 1
-- advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f  l (advance_tabstop c)
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f  l (c + 1)

-- advance_tabstop :: Int -> Int
-- advance_tabstop c = ((((c - 1) `shiftR` 3) + 1) `shiftL` 3) + 1

advancePsLoc :: PsLoc -> Char -> PsLoc
advancePsLoc (PsLoc real_loc buf_loc) c =
  PsLoc (advanceSrcLoc real_loc c) (advanceBufPos buf_loc)

advanceBufPos :: BufPos -> BufPos
advanceBufPos (BufPos i) = BufPos (i+1)

psSpanStart :: PsSpan -> PsLoc
psSpanStart (PsSpan r b) = PsLoc (realSrcSpanStart r) (bufSpanStart b)

psSpanEnd :: PsSpan -> PsLoc
psSpanEnd (PsSpan r b) = PsLoc (realSrcSpanEnd r) (bufSpanEnd b)
