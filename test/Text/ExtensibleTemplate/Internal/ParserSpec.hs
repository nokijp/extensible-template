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
      , ("abc", [TextComponent (ComponentPos 1 1) "abc"])
      , ("a{{b}}c", [TextComponent (ComponentPos 1 1) "a{b}c"])
      , ("あ{{い}}う", [TextComponent (ComponentPos 1 1) "あ{い}う"])
      , ("abc{{{xyz}}}abc", [TextComponent (ComponentPos 1 1) "abc{", FunctionComponent (ComponentPos 1 6) "xyz" [], TextComponent (ComponentPos 1 11) "}abc"])
      , ("abc{{{{xyz}}}}abc", [TextComponent (ComponentPos 1 1) "abc{{xyz}}abc"])
      , ("abc{xyz}abc", [TextComponent (ComponentPos 1 1) "abc", FunctionComponent (ComponentPos 1 4) "xyz" [], TextComponent (ComponentPos 1 9) "abc"])
      , ("abc{  xyz  }abc", [TextComponent (ComponentPos 1 1) "abc", FunctionComponent (ComponentPos 1 4) "xyz" [], TextComponent (ComponentPos 1 13) "abc"])
      , ("abc{\nxyz\n}abc", [TextComponent (ComponentPos 1 1) "abc", FunctionComponent (ComponentPos 1 4) "xyz" [], TextComponent (ComponentPos 3 2) "abc"])
      , ("a{x}b{y}c", [TextComponent (ComponentPos 1 1) "a", FunctionComponent (ComponentPos 1 2) "x" [], TextComponent (ComponentPos 1 5) "b", FunctionComponent (ComponentPos 1 6) "y" [], TextComponent (ComponentPos 1 9) "c"])
      , ("{xyz \"XXX\" 1 +1 -1 1.0 +1.0 -1.0 1e0 +1e0 -1e0 True False}", [FunctionComponent (ComponentPos 1 1) "xyz" [StringParam "XXX", IntParam 1, IntParam 1, IntParam (-1), FloatParam 1.0, FloatParam 1.0, FloatParam (-1.0), FloatParam 1.0, FloatParam 1.0, FloatParam (-1.0), BoolParam True, BoolParam False]])
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