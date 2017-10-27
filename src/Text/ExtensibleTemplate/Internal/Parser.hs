module Text.ExtensibleTemplate.Internal.Parser
  ( Component(..)
  , parseComponents
  ) where

import Control.Arrow
import Text.ExtensibleTemplate.Param
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

data Component = TextComponent String | FunctionComponent String [Param]

parseComponents :: String -> Either String [Component]
parseComponents = left show . parse parser ""

parser :: Parser [Component]
parser = many (text <|> function) <* eof

text :: Parser Component
text = TextComponent <$> many1 (nonBrace <|> brace)
  where
    nonBrace = satisfy (\c -> c /= '{' && c /= '}')
    brace = try $ ('{' <$ string "{{") <|> ('}' <$ string "}}")

function :: Parser Component
function = FunctionComponent <$> (char '{' *> spaces *> name) <*> (spaces *> (param `sepBy` spaces) <* spaces <* char '}')
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
