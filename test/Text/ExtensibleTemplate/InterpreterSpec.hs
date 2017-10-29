module Text.ExtensibleTemplate.InterpreterSpec
  ( main
  , spec
  ) where

import Control.Monad.Identity
import Control.Monad.State
import Data.IORef
import Test.Hspec
import TestUtils
import Text.ExtensibleTemplate.Extension
import Text.ExtensibleTemplate.Param

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runTemplate" $ do
    ref <- runIO $ newIORef "io"
    testRightIdentity "" ENil ""
    testRightIdentity "abc" ENil "abc"
    testRightIdentity "a{s}b{s}c" (stateExtension "s" <|< ENil) "a1b2c"
    testRightIdentity "a{s}b{s}c" (stateExtension "s" <|< stateExtension "s" <|< ENil) "a1b2c"
    testRightIdentity "a{s}b{t}c" (stateExtension "t" <|< stateExtension "s" <|< ENil) "a1b1c"
    testRightIdentity "a{s}b{t}c" (stateExtension "s" <|< stateExtension "t" <|< ENil) "a1b1c"
    testRightIdentity "a{n 0}b" (zeroExtension "n" <|< ENil) "azerob"
    testRightIdentity "a{n 0}b" (zeroExtension "n" <|< ENil) "azerob"
    testRightIO "{i}{s}" (refExtensionIO "i" ref <|< stateExtensionIO "s" <|< ENil) "io1"
    testRightIO "{i}{s}" (stateExtensionIO "s" <|< refExtensionIO "i" ref <|< ENil) "io1"
    testLeftIdentity "{" ENil
    testLeftIdentity "a{s}b{t}c" (stateExtension "s" <|< ENil)
    testLeftIdentity "a{t}b{t}c" (stateExtension "s" <|< ENil)
    testLeftIdentity "a{n 1}b" (zeroExtension "n" <|< ENil)
    testLeftIdentity "a{n 1 2}b" (zeroExtension "n" <|< ENil)
    testLeftIdentity "a{n \"0\"}b" (zeroExtension "n" <|< ENil)

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
