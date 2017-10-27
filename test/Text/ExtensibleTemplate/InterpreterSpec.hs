module Text.ExtensibleTemplate.InterpreterSpec
  ( main
  , spec
  ) where

import Control.Monad.Identity
import Control.Monad.State
import Data.Either
import Test.Hspec
import Text.ExtensibleTemplate.Extension
import Text.ExtensibleTemplate.Interpreter
import Text.ExtensibleTemplate.Param

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runTemplate" $ do
    testRight "" ENil (Right "")
    testRight "abc" ENil (Right "abc")
    testRight "a{s}b{s}c" (stateExtension "s" <|< ENil) (Right "a1b2c")
    testRight "a{s}b{s}c" (stateExtension "s" <|< stateExtension "s" <|< ENil) (Right "a1b2c")
    testRight "a{s}b{t}c" (stateExtension "t" <|< stateExtension "s" <|< ENil) (Right "a1b1c")
    testRight "a{s}b{t}c" (stateExtension "s" <|< stateExtension "t" <|< ENil) (Right "a1b1c")
    testRight "a{n 0}b" (zeroExtension "n" <|< ENil) (Right "azerob")
    testLeft "{" ENil
    testLeft "a{s}b{t}c" (stateExtension "s" <|< ENil)
    testLeft "a{t}b{t}c" (stateExtension "s" <|< ENil)
    testLeft "a{n 1}b" (zeroExtension "n" <|< ENil)
    testLeft "a{n 1 2}b" (zeroExtension "n" <|< ENil)
    testLeft "a{n \"0\"}b" (zeroExtension "n" <|< ENil)

testRight :: (RunExtensions l) => String -> l -> Either String String -> SpecWith (Arg Expectation)
testRight template extensions expectedResult = do
  actualResult <- runIO $ runTemplate extensions template
  it ("should convert " ++ show template ++ " into " ++ show expectedResult)$
    actualResult `shouldBe` expectedResult

testLeft :: (RunExtensions l) => String -> l -> SpecWith (Arg Expectation)
testLeft template extensions = do
  actualResult <- runIO $ runTemplate extensions template
  it ("should return an error when given " ++ show template)$
    actualResult `shouldSatisfy` isLeft

stateExtension :: String -> Extension (State Int)
stateExtension name = Extension
  { extensionAcceptor = (== name)
  , extensionFunction = const $ const $ do
      n <- get
      put (n + 1)
      return $ Right (show n)
  , extensionRunner = \s -> return $ evalState s 1
  }

zeroExtension :: String -> Extension Identity
zeroExtension name = Extension
  { extensionAcceptor = (== name)
  , extensionFunction = const f
  , extensionRunner = return . runIdentity
  }
    where
      f [IntParam n] = Identity $ if n == 0 then Right "zero" else Left "not zero"
      f _ = Identity $ Left "not zero"
