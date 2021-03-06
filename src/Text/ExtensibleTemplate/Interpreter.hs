{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Text.ExtensibleTemplate.Interpreter
  ( runTemplate
  , RunExtensions
  ) where

import Control.Arrow
import Control.Monad.Trans.Except
import Text.ExtensibleTemplate.Extension
import Text.ExtensibleTemplate.Internal.Parser

class Monad m => RunExtensions m l where
  runExtensions :: l -> [Component] -> ExceptT String m [Component]

instance Monad m => RunExtensions m ENil where
  runExtensions _ = return

instance (RunExtensions m t, Monad m, Monad n) => RunExtensions m (ECons m n t) where
  runExtensions (ECons e@(Extension _ _ runner) t) components = do
    step <- ExceptT $ runner $ runExceptT $ mapM (applyExtension e) components
    runExtensions t step

-- | converts a template into a string, using the specified extensions
runTemplate :: (RunExtensions m l, Monad m) => l -> String -> m (Either String String)
runTemplate extensions template = runExceptT $ do
  components <- ExceptT $ return $ parseComponents template
  evaluatedComponents <- runExtensions extensions components
  plainStrings <- ExceptT $ return $ mapM componentToEither evaluatedComponents
  return $ concat plainStrings

applyExtension :: (Monad m, Monad n) => Extension m n -> Component -> ExceptT String n Component
applyExtension _ c@(TextComponent _ _) = return c
applyExtension (Extension acceptor function _) c@(FunctionComponent pos name params) =
  if acceptor name
  then TextComponent pos <$> ExceptT (left (\s -> show pos ++ ":\n" ++ s) <$> function name params)
  else return c

componentToEither :: Component -> Either String String
componentToEither (TextComponent _ text) = Right text
componentToEither (FunctionComponent pos name _) = Left $ show pos ++ ":\nextension undefined: " ++ name
