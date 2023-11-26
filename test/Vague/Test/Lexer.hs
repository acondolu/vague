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
  let fp = "test/Vague/Test/lexer/" <> fname
  goldenVsString fname (fp <> ".golden") $ do
    bs <- ByteString.readFile (fp <> ".in")
    pure $ ByteString.fromStrict $ UTF8.fromString $ show $ Lexer.lexer bs

tests :: TestTree
tests =
  testGroup
    "Lexer"
    [ makeTest "empty"
    , makeTest "ass"
    , makeTest "curly"
    ]
