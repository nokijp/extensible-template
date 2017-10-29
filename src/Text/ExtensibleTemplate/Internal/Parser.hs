module Text.ExtensibleTemplate.Internal.Parser
  ( Component(..)
  , ComponentPos(..)
  , parseComponents
  ) where

import Control.Arrow
import Text.ExtensibleTemplate.Param
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

data ComponentPos = ComponentPos { componentLine :: Int, componentColumn :: Int } deriving (Eq, Ord)
instance Show ComponentPos where
  show (ComponentPos line column) = "(line " ++ show line ++ ", column " ++ show column ++ ")"

data Component = TextComponent ComponentPos String | FunctionComponent ComponentPos String [Param]

parseComponents :: String -> Either String [Component]
parseComponents = left show . parse parser ""

parser :: Parser [Component]
parser = many (text <|> function) <* eof

text :: Parser Component
text = TextComponent <$> getComponentPos <*> many1 (nonBrace <|> brace)
  where
    nonBrace = satisfy (\c -> c /= '{' && c /= '}')
    brace = try $ ('{' <$ string "{{") <|> ('}' <$ string "}}")

function :: Parser Component
function = FunctionComponent <$> getComponentPos <*> (char '{' *> spaces *> name) <*> (spaces *> (param `sepBy` spaces) <* spaces <* char '}')
  where
    name = (:) <$> letter <*> many alphaNum

param :: Parser Param
param =
      (StringParam <$> stringLiteral haskell)
  <|> try (FloatParam <$> (($) <$> sign <*> float haskell))
  <|> (IntParam <$> integer haskell)
  <|> (BoolParam True <$ string "True") <|> (BoolParam False <$ string "False")

sign :: Num a => Parser (a -> a)
sign = (negate <$ char '-') <|> (id <$ char '+') <|> return id

getComponentPos :: Parser ComponentPos
getComponentPos = (\pos -> ComponentPos (sourceLine pos) (sourceColumn pos)) <$> getPosition
