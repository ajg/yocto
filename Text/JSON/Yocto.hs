-- Copyright 2014 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

-- | A Minimal JSON Parser & Printer
module Text.JSON.Yocto (decode, encode, Value (..)) where

import Control.Applicative hiding ((<|>), many)
import Data.Char (chr, isControl, ord)
import Data.List (find, intercalate)
import Data.Map (fromList, Map, toList)
import Data.Maybe (fromJust)
import Data.Ratio ((%), denominator, numerator)
import Prelude hiding (exp, exponent, null)
import Numeric (fromRat, readDec, readHex, showHex)
import Text.Parsec

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
encode (String  s) = "\"" ++ concatMap escape s ++ "\""
encode (Array   a) = "[" ++ intercalate "," (encode <$> a) ++ "]"
encode (Object  o) = "{" ++ intercalate "," (f <$> toList o) ++ "}"
  where f (n, v) = encode (String n) ++ ":" ++ encode v

escape c = maybe control (\e -> '\\' : [e]) (c `lookup` escapes) where
  control = if isControl c then (escape' . showHex . ord) c else [c]
  escape' hex = "\\u" ++ replicate (4 - length s) '0' ++ s where s = hex ""
escapes = [('\b', 'b'), ('\f', 'f'), ('\n', 'n'), ('\r', 'r'),
           ('\t', 't'), ('\\', '\\'), ('"', '"')]

-- | Decodes a 'Value' from a 'String'.
decode :: String -> Value
decode = attempt . parse input "JSON"
    where attempt (Right (success, "")) = success
          attempt (Right (_, trail)) = error $ "trailing " ++ show trail
          attempt (Left failure) = error $ "invalid " ++ show failure

input = value & getInput where
  value = lexical $ null <|> boolean <|> number <|> string' <|> array <|> object

  null    = Null    <$  string "null"
  boolean = Boolean <$> (True <$ string "true" <|> False <$ string "false")
  number  = Number  <$> rational <$> (integer & fraction & exponent)
  string' = String  <$> between (char '"') (char '"') (many character)
  array   = Array   <$> between (char '[') (char ']') (listOf value)
  object  = Object  <$> between (char '{') (char '}') (fromList <$> listOf pair)

  pair = lexical name & (lexical (char ':') >> value)
    where name = (\(String s) -> s) <$> string'
  character = escaped <|> satisfy (not . \c -> isControl c || elem c "\"\\")
    where escaped  = char '\\' >> (unescape <$> oneOf "\"\\/bfnrt" <|> unicode)
          unicode  = char 'u' >> (hexadecimal <$> count 4 hexDigit)
          unescape c = fst . fromJust $ find ((== c) . snd) escapes

  integer  = option '+' (char '-') & (0 <$ char '0' <|> natural)
  fraction = option 0 (char '.' >> fractional <$> many1 digit)
  exponent = option 0 (oneOf "eE" >> natural `maybeSignedWith` (plus <|> minus))
    where number `maybeSignedWith` sign = ($ 0) <$> option (+) sign <*> number
          (plus, minus) = ((+) <$ char '+', (-) <$ char '-')

  a & b   = (,) <$> a <*> b
  listOf  = (`sepBy` char ',')
  lexical = between ws ws where ws = many (oneOf " \t\r\n")
  natural = decimal <$> many1 digit
  decimal = fst . head . readDec
  hexadecimal = chr . fst . head . readHex
  fractional digits = decimal digits % (10 ^ length digits)
  rational ((('+', int), frac), exp) = (fromInteger int + frac) * 10 ^^ exp
  rational ((('-', int), frac), exp) = -(fromInteger int + frac) * 10 ^^ exp
