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
  { extensionAcceptor :: String -> Bool  -- ^ the tester that returns True iff the name is processible by the extension
  , extensionFunction :: String -> [Param] -> n (Either String String)  -- ^ receives the name of the function and parameters, and returns a component monad
  , extensionRunner :: forall a. n a -> m a  -- ^ the runner that converts the result constructed by 'extensionFunction' into a type @m@
  }

type ExtensionIO = Extension IO
type ExtensionId = Extension Identity

class Monad m => ExtensionList m l

-- | the empty extension list
data ENil = ENil
instance Monad m => ExtensionList m ENil

-- | a non-empty extension list
data ECons m n l = ECons (Extension m n) l
instance (ExtensionList m t, Monad m, Monad n) => ExtensionList m (ECons m n t)

-- | a support function to construct extension lists
infixr 5 <|<
(<|<) :: ExtensionList m l => Extension m n -> l -> ECons m n l
(<|<) = ECons
