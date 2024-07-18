{-# LANGUAGE OverloadedStrings #-}

module Vague.Test.Parser (tests) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8
import Test.Tasty
import Test.Tasty.Golden
import qualified Vague.Lexer as Lexer
import qualified Vague.Parser as Parser
import Prelude

makeTest :: String -> TestTree
makeTest fname = do
  let dir = "test/Vague/Test/"
  goldenVsString fname (dir <> "parser/" <> fname <> ".golden") $ do
    bs <- ByteString.readFile (dir <> "tests/" <> fname <> ".in")
    pure $ ByteString.fromStrict $ UTF8.fromString $ show $ Parser.parse $ Lexer.lexer bs

tests :: TestTree
tests =
  testGroup "Parser" $
    map
      makeTest
      [ "0",
        "ass",
        "curly",
        "str",
        "def",
        "6",
        "7",
        "8"
      ]
