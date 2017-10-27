module Text.ExtensibleTemplate.Interpreter
  ( runTemplate
  , RunExtensions
  ) where

import Control.Monad.Trans.Except
import Text.ExtensibleTemplate.Extension
import Text.ExtensibleTemplate.Internal.Parser

class RunExtensions l where
  runExtensions :: l -> [Component] -> ExceptT String IO [Component]

instance RunExtensions ENil where
  runExtensions _ = return

instance (RunExtensions t, Monad m) => RunExtensions (ECons m t) where
  runExtensions (ECons e@(Extension _ _ runner) t) components = do
    step <- ExceptT $ runner $ runExceptT $ mapM (applyExtension e) components
    runExtensions t step

runTemplate :: (RunExtensions l) => l -> String -> IO (Either String String)
runTemplate extensions template = runExceptT $ do
  components <- ExceptT $ return $ parseComponents template
  evaluatedComponents <- runExtensions extensions components
  plainStrings <- ExceptT $ return $ mapM componentToEither evaluatedComponents
  return $ concat plainStrings

applyExtension :: Monad m => Extension m -> Component -> ExceptT String m Component
applyExtension _ c@(TextComponent _) = return c
applyExtension (Extension acceptor function _) c@(FunctionComponent name params) =
  if acceptor name
  then TextComponent <$> ExceptT (function name params)
  else return c

componentToEither :: Component -> Either String String
componentToEither (TextComponent text) = Right text
componentToEither (FunctionComponent name _) = Left $ "extension undefined: " ++ name
