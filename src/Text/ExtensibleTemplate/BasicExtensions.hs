{-# LANGUAGE FlexibleContexts #-}

module Text.ExtensibleTemplate.BasicExtensions
  ( counterExtension
  , counterExtensionIO
  , counterExtensionGen
  , valueExtension
  , valueExtensionIO
  , valueExtensionGen
  , includeExtensionIO
  , includeExtensionGen
  ) where

import Control.Arrow
import Control.Exception
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import System.FilePath
import Text.ExtensibleTemplate

counterExtension :: Extension Identity (State (Map Param Int))
counterExtension = counterExtensionGen

counterExtensionIO :: Extension IO (State (Map Param Int))
counterExtensionIO = counterExtensionGen

counterExtensionGen :: Monad m => Extension m (State (Map Param Int))
counterExtensionGen = Extension
  { extensionAcceptor = (== name)
  , extensionFunction = const f
  , extensionRunner = \s -> return $ evalState s M.empty
  } where
    f [p] = do
      table <- get
      let n = M.findWithDefault 1 p table
      put $ M.insert p (n + 1) table
      return $ Right $ show n
    f _ = return $ Left $ show name ++ " must have one parameter"
    name = "counter"


valueExtension :: Map String String -> ExtensionId Identity
valueExtension = valueExtensionGen

valueExtensionIO :: Map String String -> ExtensionIO Identity
valueExtensionIO = valueExtensionGen

valueExtensionGen :: Monad m => Map String String -> Extension m Identity
valueExtensionGen table = Extension
  { extensionAcceptor = (== name)
  , extensionFunction = const f
  , extensionRunner = return . runIdentity
  } where
    f [StringParam key] = return $ maybe (Left $ "undefined value: " ++ show key) Right $ M.lookup key table
    f _ = return $ Left $ show name ++ " must have one string parameter"
    name = "value"


includeExtensionIO :: FilePath -> ExtensionIO IO
includeExtensionIO = includeExtensionGen

includeExtensionGen :: MonadIO m => FilePath -> Extension m IO
includeExtensionGen basedir = Extension
  { extensionAcceptor = (== name)
  , extensionFunction = const f
  , extensionRunner = liftIO
  } where
    f [StringParam file] = left (show :: IOError -> String) <$> try (readFile (basedir </> file))
    f _ = return $ Left $ show name ++ " must have one string parameter"
    name = "include"
