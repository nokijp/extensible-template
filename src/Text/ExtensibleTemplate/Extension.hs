{-# LANGUAGE RankNTypes #-}

module Text.ExtensibleTemplate.Extension
  ( Extension(..)
  , ExtensionList
  , ENil(..)
  , ECons(..)
  , (<|<)
  ) where

import Text.ExtensibleTemplate.Param

data Extension m = Extension
  { extensionAcceptor :: String -> Bool
  , extensionFunction :: String -> [Param] -> m (Either String String)
  , extensionRunner :: forall a. m a -> IO a
  }

class ExtensionList l

data ENil = ENil
instance ExtensionList ENil

data ECons m l = ECons (Extension m) l
instance (ExtensionList t, Monad m) => ExtensionList (ECons m t)

infixr 5 <|<
(<|<) :: ExtensionList l => Extension m -> l -> ECons m l
(<|<) = ECons
