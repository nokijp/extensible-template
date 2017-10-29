{-# LANGUAGE FlexibleContexts #-}

module Text.ExtensibleTemplate.InterpreterSpec
  ( main
  , spec
  ) where

import Control.Monad.Identity
import Control.Monad.State
import Data.Either
import Data.IORef
import Test.Hspec
import Text.ExtensibleTemplate.Extension
import Text.ExtensibleTemplate.Interpreter
import Text.ExtensibleTemplate.Param

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runTemplate" $ do
    ref <- runIO $ newIORef "io"
    testRightIdentity "" ENil (Right "")
    testRightIdentity "abc" ENil (Right "abc")
    testRightIdentity "a{s}b{s}c" (stateExtension "s" <|< ENil) (Right "a1b2c")
    testRightIdentity "a{s}b{s}c" (stateExtension "s" <|< stateExtension "s" <|< ENil) (Right "a1b2c")
    testRightIdentity "a{s}b{t}c" (stateExtension "t" <|< stateExtension "s" <|< ENil) (Right "a1b1c")
    testRightIdentity "a{s}b{t}c" (stateExtension "s" <|< stateExtension "t" <|< ENil) (Right "a1b1c")
    testRightIdentity "a{n 0}b" (zeroExtension "n" <|< ENil) (Right "azerob")
    testRightIdentity "a{n 0}b" (zeroExtension "n" <|< ENil) (Right "azerob")
    testRightIO "{i}{s}" (refExtensionIO "i" ref <|< stateExtensionIO "s" <|< ENil) (Right "io1")
    testRightIO "{i}{s}" (stateExtensionIO "s" <|< refExtensionIO "i" ref <|< ENil) (Right "io1")
    testLeftIdentity "{" ENil
    testLeftIdentity "a{s}b{t}c" (stateExtension "s" <|< ENil)
    testLeftIdentity "a{t}b{t}c" (stateExtension "s" <|< ENil)
    testLeftIdentity "a{n 1}b" (zeroExtension "n" <|< ENil)
    testLeftIdentity "a{n 1 2}b" (zeroExtension "n" <|< ENil)
    testLeftIdentity "a{n \"0\"}b" (zeroExtension "n" <|< ENil)

testRightIdentity :: RunExtensions Identity l => String -> l -> Either String String -> SpecWith (Arg Expectation)
testRightIdentity template extensions expectedResult = do
  let actualResult = runIdentity $ runTemplate extensions template
  it ("should convert " ++ show template ++ " into " ++ show expectedResult)$
    actualResult `shouldBe` expectedResult

testLeftIdentity :: RunExtensions Identity l => String -> l -> SpecWith (Arg Expectation)
testLeftIdentity template extensions = do
  let actualResult = runIdentity $ runTemplate extensions template
  it ("should return an error when given " ++ show template)$
    actualResult `shouldSatisfy` isLeft

testRightIO :: RunExtensions IO l => String -> l -> Either String String -> SpecWith (Arg Expectation)
testRightIO template extensions expectedResult = do
  actualResult <- runIO $ runTemplate extensions template
  it ("should convert " ++ show template ++ " into " ++ show expectedResult)$
    actualResult `shouldBe` expectedResult

stateExtension :: String -> ExtensionId (State Int)
stateExtension = stateExtensionGen
stateExtensionIO :: String -> ExtensionIO (State Int)
stateExtensionIO = stateExtensionGen
stateExtensionGen :: Monad m => String -> Extension m (State Int)
stateExtensionGen name = Extension
  { extensionAcceptor = (== name)
  , extensionFunction = const $ const $ do
      n <- get
      put (n + 1)
      return $ Right (show n)
  , extensionRunner = \s -> return $ evalState s 1
  }

zeroExtension :: String -> ExtensionId Identity
zeroExtension = zeroExtensionGen
zeroExtensionGen :: Monad m => String -> Extension m Identity
zeroExtensionGen name = Extension
  { extensionAcceptor = (== name)
  , extensionFunction = const f
  , extensionRunner = return . runIdentity
  } where
    f [IntParam n] = Identity $ if n == 0 then Right "zero" else Left "not zero"
    f _ = Identity $ Left "not zero"

refExtensionIO :: String -> IORef String -> ExtensionIO IO
refExtensionIO name ref = Extension
  { extensionAcceptor = (== name)
  , extensionFunction = const $ const $ Right <$> readIORef ref
  , extensionRunner = id
  }
