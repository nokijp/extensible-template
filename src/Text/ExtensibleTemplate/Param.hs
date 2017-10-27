module Text.ExtensibleTemplate.Param
  ( Param(..)
  ) where

data Param = StringParam String | IntParam Integer | FloatParam Double | BoolParam Bool deriving (Eq, Show, Ord)
