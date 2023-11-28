{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import qualified Vague.Test.Lexer
import qualified Vague.Test.Parser

tests :: TestTree
tests =
  testGroup
    "Vague"
    [ Vague.Test.Lexer.tests,
      Vague.Test.Parser.tests
    ]

main :: IO ()
main = defaultMain tests
