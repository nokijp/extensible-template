{-# LANGUAGE FlexibleContexts #-}

module TestUtils
  ( testRightIdentity
  , testLeftIdentity
  , testRightIO
  , testLeftIO
  ) where

import Control.Monad.Identity
import Data.Either
import Test.Hspec
import Text.ExtensibleTemplate.Interpreter

testRightIdentity :: RunExtensions Identity l => String -> l -> String -> SpecWith (Arg Expectation)
testRightIdentity template extensions expectedResult = do
  let actualResult = runIdentity $ runTemplate extensions template
  it ("should convert " ++ show template ++ " into " ++ show expectedResult) $
    actualResult `shouldBe` Right expectedResult

testLeftIdentity :: RunExtensions Identity l => String -> l -> SpecWith (Arg Expectation)
testLeftIdentity template extensions = do
  let actualResult = runIdentity $ runTemplate extensions template
  it ("should return an error when given " ++ show template) $
    actualResult `shouldSatisfy` isLeft

testRightIO :: RunExtensions IO l => String -> l -> String -> SpecWith (Arg Expectation)
testRightIO template extensions expectedResult = do
  actualResult <- runIO $ runTemplate extensions template
  it ("should convert " ++ show template ++ " into " ++ show expectedResult) $
    actualResult `shouldBe` Right expectedResult

testLeftIO :: RunExtensions IO l => String -> l -> SpecWith (Arg Expectation)
testLeftIO template extensions = do
  actualResult <- runIO $ runTemplate extensions template
  it ("should return an error when given " ++ show template) $
    actualResult `shouldSatisfy` isLeft
