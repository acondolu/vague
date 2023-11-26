{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import qualified Vague.Test.Lexer

tests :: TestTree
tests =
  testGroup
    "Vague"
    [ Vague.Test.Lexer.tests
    ]

main :: IO ()
main = defaultMain tests
