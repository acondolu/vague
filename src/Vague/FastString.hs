module Vague.FastString (FastString, mkFastString, toText, fsLit) where

import Data.Text (Text)
import qualified Data.Text as T

-- import qualified Data.HashMap.Strict as HashMap

-- newtype FastString = FastString Int
newtype FastString = FastString Text
  deriving newtype (Eq, Ord, Show)

mkFastString :: Text -> FastString
mkFastString = FastString

toText :: FastString -> Text
toText (FastString s) = s

fsLit :: String -> FastString
fsLit = FastString . T.pack