{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.ExtensibleTemplate.Internal.ParserSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Either
import Test.Hspec
import Text.ExtensibleTemplate.Internal.Parser
import Text.ExtensibleTemplate.Param

deriving instance Eq Component
deriving instance Show Component

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseComponents" $ do
    forM_
      [ ("", [])
      , ("abc", [TextComponent "abc"])
      , ("a{{b}}c", [TextComponent "a{b}c"])
      , ("あ{{い}}う", [TextComponent "あ{い}う"])
      , ("abc{{{xyz}}}abc", [TextComponent "abc{", FunctionComponent "xyz" [], TextComponent "}abc"])
      , ("abc{{{{xyz}}}}abc", [TextComponent "abc{{xyz}}abc"])
      , ("abc{xyz}abc", [TextComponent "abc", FunctionComponent "xyz" [], TextComponent "abc"])
      , ("abc{  xyz  }abc", [TextComponent "abc", FunctionComponent "xyz" [], TextComponent "abc"])
      , ("abc{\nxyz\n}abc", [TextComponent "abc", FunctionComponent "xyz" [], TextComponent "abc"])
      , ("a{x}b{y}c", [TextComponent "a", FunctionComponent "x" [], TextComponent "b", FunctionComponent "y" [], TextComponent "c"])
      , ("{xyz \"XXX\" 1 +1 -1 1.0 +1.0 -1.0 1e0 +1e0 -1e0 True False}", [FunctionComponent "xyz" [StringParam "XXX", IntParam 1, IntParam 1, IntParam (-1), FloatParam 1.0, FloatParam 1.0, FloatParam (-1.0), FloatParam 1.0, FloatParam 1.0, FloatParam (-1.0), BoolParam True, BoolParam False]])
      ] $ \(template, components) ->
        it ("should parse " ++ show template ++ " into " ++ show components) $
          parseComponents template `shouldBe` Right components
    forM_
      [ "{"
      , "}"
      , "{ { a } }"
      , "{}"
      , "{1}"
      , "{a ++1}"
      , "{a --1}"
      , "{a ++1.0}"
      , "{a --1.0}"
      ] $ \template ->
        it ("should return an error when given " ++ show template) $
          parseComponents template `shouldSatisfy` isLeft