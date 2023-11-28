module Vague.FastString (FastString, mkFastString, toText, fsLit, fromByteString) where

import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)

-- import qualified Data.HashMap.Strict as HashMap

-- newtype FastString = FastString Int
newtype FastString = FastString Text
  deriving newtype (Eq, Ord, Show, IsString)

mkFastString :: Text -> FastString
mkFastString = FastString

fromByteString :: ByteString -> FastString
fromByteString bs = FastString $ decodeUtf8Lenient bs

toText :: FastString -> Text
toText (FastString s) = s

fsLit :: String -> FastString
fsLit = FastString . T.pack
