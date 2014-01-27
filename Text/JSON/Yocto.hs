-- Copyright 2014 Alvaro J. Genial [http://alva.ro]; see LICENSE file for more.

module Text.JSON.Yocto (Value (..)) where

import Control.Applicative hiding ((<|>), many)
import Data.Char (isControl)
import Data.List (intercalate)
import Data.Ratio ((%), denominator, numerator)
import Prelude hiding (exp, exponent, or, null, showChar)
import Numeric (readDec, readHex, showHex)
import qualified Text.Parsec as Parse
import Text.Parsec hiding (string, token)
import Text.Printf (printf)

data Value = Null
           | Boolean Bool
           | Number Rational
           | String [Char]
           | Array  [Value]
           | Object [(String, Value)]
  deriving (Eq, Ord)

--- Reading --------------------------------------------------------------------

instance Read Value where
  readsPrec _ string = attempt (parse (syntax & getInput) "JSON" string)
    where attempt (Left failure) = error ("invalid " ++ show failure)
          attempt (Right success) = [success]

syntax = whitespace >> value where

  whitespace = many (oneOf " \t\r\n")
  value = null <|> string <|> array <|> object <|> number <|> boolean

  null    = Null    <$  keyword "null"
  boolean = Boolean <$> (True <$ keyword "true" <|> False <$ keyword "false")
  number  = Number  <$> rational <$> lexical (integer & fraction & exponent)
  string  = String  <$> many character `enclosedBy` (char,  '"', '"')
  array   = Array   <$> commaSep value `enclosedBy` (token, '[', ']')
  object  = Object  <$> commaSep pair  `enclosedBy` (token, '{', '}')

  pair = name & (token ':' >> value) where name = (\(String s) -> s) <$> string
  character = satisfy (\c -> not (isControl c) && c /= '"' && c /= '\\')
          <|> (char '\\' >> ((char 'u' >> ordinal <$> count 4 hexDigit)
                        <|> oneOf "\"\\/bfnrt"))

  integer  = fromInteger <$> (0 <$ char '0' <|> natural) `maybeSignedWith` minus
  fraction = option 0 (char '.' >> fmap fractional (many1 digit))
  exponent = option 0 (oneOf "eE" >> natural `maybeSignedWith` (plus <|> minus))

  commaSep = (`sepBy` token ',')
  items `enclosedBy` (term, start, end) = term start *> items <* term end
  n `maybeSignedWith` sign = ($ 0) <$> option (+) sign <*> n
  (plus, minus) = ((+) <$ char '+', (-) <$ char '-')

  lexical  = (<* whitespace)
  integral = fst . head . readDec
  ordinal  = toEnum . fst . head . readHex

  natural = integral <$> many1 digit
  fractional digits = integral digits % (10 ^ length digits)
  rational ((int, frac), exp) = (int + (signum int * frac)) * 10 ^^ exp

  token = lexical . Parse.char
  keyword = lexical . Parse.string

--- Showing --------------------------------------------------------------------

instance Show Value where
  show  Null           = "null"
  show (Boolean b)     = if b then "true" else "false"
  show (Number r)      = showRational r
  show (String chars)  = "\"" ++ concat (showChar <$> chars) ++ "\""
  show (Array values)  = "[" ++ intercalate "," (show <$> values) ++ "]"
  show (Object pairs)  = "{" ++ intercalate "," (showPair <$> pairs) ++ "}"

showPair (name, value) = show name ++ ":" ++ show value
showChar c = maybe control (\e -> "\\" ++ [e]) (c `lookup` exceptions)
  where control = if isControl c then (encode . showHex . fromEnum) c else [c]
        encode hex = "\\u" ++ replicate (4 - length s) '0' ++ s where s = hex ""
        exceptions = [('\b', 'b'), ('\f', 'f'), ('\n', 'n'), ('\r', 'r'),
                      ('\t', 't'), ('\\', '\\'), ('"', '"')]

showRational r | remainder == 0 = show whole
               | otherwise = printf "%f" (fromRational r :: Double)
  where (whole, remainder) = (numerator r) `divMod` (denominator r)

-- Give more intuitive names to the Functor combinators:
a & b = (,) <$> a <*> b
