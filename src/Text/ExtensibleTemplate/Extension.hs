{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}

module Text.ExtensibleTemplate.Extension
  ( Extension(..)
  , ExtensionIO
  , ExtensionId
  , ExtensionList
  , ENil(..)
  , ECons(..)
  , (<|<)
  ) where

import Control.Monad.Identity
import Text.ExtensibleTemplate.Param

data Extension m n = Extension
  { extensionAcceptor :: String -> Bool
  , extensionFunction :: String -> [Param] -> n (Either String String)
  , extensionRunner :: forall a. n a -> m a
  }

type ExtensionIO = Extension IO
type ExtensionId = Extension Identity

class Monad m => ExtensionList m l

data ENil = ENil
instance Monad m => ExtensionList m ENil

data ECons m n l = ECons (Extension m n) l
instance (ExtensionList m t, Monad m, Monad n) => ExtensionList m (ECons m n t)

infixr 5 <|<
(<|<) :: ExtensionList m l => Extension m n -> l -> ECons m n l
(<|<) = ECons
