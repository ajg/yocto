-- Copyright 2014 Alvaro J. Genial [http://alva.ro]; see LICENSE file for more.
-- | A Minimal JSON Parser & Printer
module Text.JSON.Yocto (decode, encode, Value (..)) where

import Control.Applicative hiding ((<|>), many)
import Data.Char (isControl)
import Data.List (intercalate)
import Data.Map (fromList, Map, toList)
import Data.Ratio ((%), denominator, numerator)
import Prelude hiding (exp, exponent, null)
import Numeric (fromRat, readDec, readHex, showHex)
import Text.Parsec hiding (string, token)
import qualified Text.Parsec as Parsec

-- | Represents arbitrary JSON data.
data Value = Null
           | Boolean Bool
           | Number  Rational
           | String  String
           | Array   [Value]
           | Object  (Map String Value) deriving (Eq, Ord, Read, Show)

-- | Encodes a 'Value' to a 'String'.
encode :: Value -> String
encode  Null       = "null"
encode (Boolean b) = if b then "true" else "false"
encode (Number  n) = if rem == 0 then show i else show $ fromRat n
  where (i, rem) = numerator n `divMod` denominator n
encode (String  s) = "\"" ++ concat (escape <$> s) ++ "\""
encode (Array   a) = "[" ++ intercalate "," (encode <$> a) ++ "]"
encode (Object  o) = "{" ++ intercalate "," (f <$> toList o) ++ "}"
  where f (n, v) = encode (String n) ++ ":" ++ encode v

escape c = maybe control (\e -> '\\' : [e]) (c `lookup` exceptions) where
  control = if isControl c then (encode . showHex . fromEnum) c else [c]
  encode hex = "\\u" ++ replicate (4 - length s) '0' ++ s where s = hex ""
  exceptions = [('\b', 'b'), ('\f', 'f'), ('\n', 'n'), ('\r', 'r'),
                ('\t', 't'), ('\\', '\\'), ('"', '"')]

-- | Decodes a 'Value' from a 'String'.
decode :: String -> Value
decode = attempt . parse input "JSON"
    where attempt (Right (success, "")) = success
          attempt (Right (_, trail)) = error $ "trailing " ++ show trail
          attempt (Left failure) = error $ "invalid " ++ show failure

input = (whitespace >> value) & getInput where
  value = null <|> boolean <|> number <|> string <|> array <|> object

  null    = Null    <$  keyword "null"
  boolean = Boolean <$> (True <$ keyword "true" <|> False <$ keyword "false")
  number  = Number  <$> rational <$> lexical (integer & fraction & exponent)
  string  = String  <$> many character `within` (char,  '"', '"')
  array   = Array   <$> commaSep value `within` (token, '[', ']')
  object  = Object  <$> fromList <$> commaSep pair `within` (token, '{', '}')

  pair = name & (token ':' >> value) where name = (\(String s) -> s) <$> string
  character = escape <|> satisfy (not . \c -> isControl c || elem c "\"\\")
    where escape  = char '\\' >> (oneOf "\"\\/bfnrt" <|> unicode)
          unicode = char 'u' >> ordinal <$> count 4 hexDigit

  integer  = option '+' (char '-') & (0 <$ char '0' <|> natural)
  fraction = option 0 (char '.' >> fractional <$> many1 digit)
  exponent = option 0 (oneOf "eE" >> natural `maybeSignedWith` (plus <|> minus))
    where (plus, minus) = ((+) <$ char '+', (-) <$ char '-')

  items `within` (term, start, end) = term start *> items <* term end
  number `maybeSignedWith` sign = ($ 0) <$> option (+) sign <*> number
  (token, keyword) = (lexical . Parsec.char, lexical . Parsec.string)

  a & b      = (,) <$> a <*> b
  commaSep   = (`sepBy` token ',')
  whitespace = many (oneOf " \t\r\n")
  lexical    = (<* whitespace)
  integral   = fst . head . readDec
  ordinal    = toEnum . fst . head . readHex
  natural    = integral <$> many1 digit
  fractional digits = integral digits % (10 ^ length digits)
  rational ((('+', int), frac), exp) = (fromInteger int + frac) * 10 ^^ exp
  rational ((('-', int), frac), exp) = -(fromInteger int + frac) * 10 ^^ exp
