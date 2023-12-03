{-# LANGUAGE OverloadedStrings #-}

module Vague.Test.Lexer (tests) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8
import Test.Tasty
import Test.Tasty.Golden
import qualified Vague.Lexer as Lexer
import Prelude

makeTest :: String -> TestTree
makeTest fname = do
  let dir = "test/Vague/Test/"
  goldenVsString fname (dir <> "lexer/" <> fname <> ".golden") $ do
    bs <- ByteString.readFile (dir <> "tests/" <> fname <> ".in")
    pure $ ByteString.fromStrict $ UTF8.fromString $ show $ Lexer.lexer bs

tests :: TestTree
tests =
  testGroup
    "Lexer"
    [ makeTest "empty",
      makeTest "ass",
      makeTest "curly",
      makeTest "str",
      makeTest "def",
      makeTest "6"
    ]
