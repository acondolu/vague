{-# LANGUAGE OverloadedStrings #-}

module Vague.FastString (FastString, fromByteString, fsShow) where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.UTF8 as UTF8
import Data.HashMap.Internal (hash)
import qualified Data.HashMap.Internal as HashMap
import Data.IORef
import Data.String (IsString (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)
import GHC.IO (unsafePerformIO)

data FastString = FastString {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord)

instance Show FastString where
  show (FastString i j) = show $! toBS i j

fsShow :: FastString -> String
fsShow x@(FastString i j) = "FastString " <> show i <> " " <> show j <> " (" <> show x <> ")"

instance IsString FastString where
  fromString = fromByteString . UTF8.fromString

fromByteString :: ByteString -> FastString
fromByteString bs =
  let (i, j) = lookupOrInsert bs
   in FastString i j

-----

toBS :: Int -> Int -> ByteString
toBS i j = unsafePerformIO $ do
  pool <- readIORef bsRef
  pure $! ByteString.drop i $ ByteString.take j pool

{-# NOINLINE hmVar #-}
hmVar :: MVar (HashMap.HashMap Word [(Int, Int)])
hmVar = unsafePerformIO $ newMVar HashMap.empty

{-# NOINLINE lookupOrInsert #-}
lookupOrInsert :: ByteString -> (Int, Int)
lookupOrInsert bs = unsafePerformIO $ modifyMVar hmVar $ \hm -> do
  let h = hash bs
      check :: ByteString -> [(Int, Int)] -> IO (Maybe (Int, Int))
      check _ [] = pure Nothing
      check pool ((i, j) : ijs) =
        eqInPool bs pool i j >>= \case
          True -> pure $ Just (i, j)
          False -> check pool ijs
      insert ixs pool ix = do
        let n = ix + ByteString.length bs
            poolLen = ByteString.length pool
        if n <= poolLen
          then do
            bsCopy pool bs ix
            writeIORef ixRef n
            let hm' = HashMap.unsafeInsert h ((ix, n) : ixs) hm
            pure (hm', (ix, n))
          else do
            -- expand pool
            pool' <- mkPool pool (2 * poolLen)
            writeIORef bsRef pool'
            insert ixs pool' ix
  pool <- readIORef bsRef
  ix <- readIORef ixRef
  case HashMap.lookup h hm of
    Just ixs ->
      check pool ixs >>= \case
        Just (i, j) -> pure (hm, (i, j))
        Nothing -> insert ixs pool ix
    Nothing -> insert [] pool ix

eqInPool :: ByteString -> ByteString -> Int -> Int -> IO Bool
eqInPool (BSI.BS bs bsLen) (BSI.BS pool _) i j
  | bsLen == (j - i) = do
      BSI.unsafeWithForeignPtr bs $ \bsPtr ->
        BSI.unsafeWithForeignPtr pool $ \poolPtr -> do
          x <- BSI.memcmp bsPtr (poolPtr `plusPtr` i) bsLen
          return $! x == 0
  | otherwise = pure False

{-# NOINLINE ixRef #-}
ixRef :: IORef Int
ixRef = unsafePerformIO $ newIORef 0

{-# NOINLINE bsRef #-}
bsRef :: IORef ByteString
bsRef = unsafePerformIO $ mkPool "" 1024 >>= newIORef

bsCopy :: ByteString -> ByteString -> Int -> IO ()
bsCopy (BSI.BS dst _) (BSI.BS src j) i = do
  withForeignPtr src $ \srcPtr ->
    withForeignPtr dst $ \destPtr ->
      copyBytes (destPtr `plusPtr` i) srcPtr j

mkPool :: ByteString -> Int -> IO ByteString
mkPool (BSI.BS orig origSize) n = do
  ptr <- BSI.mallocByteString n
  withForeignPtr orig $ \srcPtr ->
    withForeignPtr ptr $ \destPtr ->
      copyBytes destPtr srcPtr origSize
  BSI.mkDeferredByteString ptr n
