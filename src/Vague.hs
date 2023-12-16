module Vague () where

import Vague.Lexer ()

{-
resolve :: Context -> Located QualModuleName -> IO (Either Error FilePath)
FilePath -> IO (Either Error ByteString)
parse :: ByteString -> Either PsError Module
typecheck :: TcContext -> PsModule -> Either TcError TcModule
compile :: CmContext -> TcModule -> Either CmError CmModule
-}
