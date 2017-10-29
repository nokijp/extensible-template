{-# LANGUAGE FlexibleContexts #-}

module Text.ExtensibleTemplate.BasicExtensionsSpec
  ( main
  , spec
  ) where

import qualified Data.Map.Strict as M
import Test.Hspec
import TestUtils
import Text.ExtensibleTemplate.BasicExtensions
import Text.ExtensibleTemplate.Extension

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "counterExtension" $ do
    testRightIdentity "" (counterExtension <|< ENil) ""
    testRightIdentity "{counter 1}{counter \"str\"}{counter 1}{counter 1}{counter \"str\"}" (counterExtension <|< ENil) "11232"
    testLeftIdentity "{counter}" (counterExtension <|< ENil)
    testLeftIdentity "{counter 1 1}" (counterExtension <|< ENil)

  describe "valueExtension" $ do
    let values = M.fromList [("one", "1"), ("two", "2")]
    testRightIdentity "" (valueExtension M.empty <|< ENil) ""
    testRightIdentity "" (valueExtension values <|< ENil) ""
    testRightIdentity "{value \"one\"}{value \"one\"}{value \"two\"}" (valueExtension values <|< ENil) "112"
    testLeftIdentity "{value}" (valueExtension values <|< ENil)
    testLeftIdentity "{value \"undefined\"}" (valueExtension values <|< ENil)
    testLeftIdentity "{value \"one\" \"two\"}" (valueExtension values <|< ENil)

  describe "includeExtensionIO" $ do
    let resourcePath = "test/resources/"
    testRightIO "" (includeExtensionIO resourcePath <|< ENil) ""
    testRightIO "{include \"include-test-file\"}" (includeExtensionIO resourcePath <|< ENil) "abc\ndef\nghi\n"
    testLeftIO "{include \"include-test-file\"}" (includeExtensionIO "." <|< ENil)
    testLeftIO "{include \"non-existing-file\"}" (includeExtensionIO resourcePath <|< ENil)
    testLeftIO "{include}" (includeExtensionIO resourcePath <|< ENil)
    testLeftIO "{include 1}" (includeExtensionIO resourcePath <|< ENil)
    testLeftIO "{include \"include-test-file\" \"include-test-file\"}" (includeExtensionIO resourcePath <|< ENil)
