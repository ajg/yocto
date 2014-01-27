-- Copyright 2014 Alvaro J. Genial [http://alva.ro]; see LICENSE file for more.
module Text.JSON.Yocto (Value (..)) where

import Control.Applicative hiding ((<|>), many)
import Data.Char (isControl)
import Data.List (intercalate)
import Data.Map (fromList, Map, toList)
import Data.Ratio ((%), denominator, numerator)
import Prelude hiding (exp, exponent, null)
import Numeric (fromRat, readDec, readHex, showHex)
import Text.Parsec hiding (string, token)
import qualified Text.Parsec as Parsec

data Value = Null
           | Boolean Bool
           | Number  Rational
           | String  [Char]
           | Array   [Value]
           | Object  (Map String Value)
  deriving (Eq, Ord)

instance Show Value where
  show  Null       = "null"
  show (Boolean b) = if b then "true" else "false"
  show (Number  n) = show $ fromRat n
  show (String  s) = "\"" ++ concat (escape <$> s) ++ "\""
  show (Array   a) = "[" ++ intercalate "," (show <$> a) ++ "]"
  show (Object  o) = "{" ++ intercalate "," (f <$> toList o) ++ "}"
    where f (n, v) = show n ++ ":" ++ show v

escape c = maybe control (\e -> "\\" ++ [e]) (c `lookup` exceptions) where
  control = if isControl c then (encode . showHex . fromEnum) c else [c]
  encode hex = "\\u" ++ replicate (4 - length s) '0' ++ s where s = hex ""
  exceptions = [('\b', 'b'), ('\f', 'f'), ('\n', 'n'), ('\r', 'r'),
                ('\t', 't'), ('\\', '\\'), ('"', '"')]

instance Read Value where
  readsPrec _ string = attempt $ parse input "JSON" string
    where attempt (Left failure) = error $ "invalid " ++ show failure
          attempt (Right success) = [success]

input = (whitespace >> value) & getInput where
  value = null <|> string <|> array <|> object <|> number <|> boolean

  null    = Null    <$  keyword "null"
  boolean = Boolean <$> (True <$ keyword "true" <|> False <$ keyword "false")
  number  = Number  <$> rational <$> lexical (integer & fraction & exponent)
  string  = String  <$> many character `within` (char,  '"', '"')
  array   = Array   <$> commaSep value `within` (token, '[', ']')
  object  = Object  <$> fromList <$> commaSep pair `within` (token, '{', '}')

  pair = name & (token ':' >> value) where name = (\(String s) -> s) <$> string
  character = satisfy (not . \c -> isControl c || elem c "\"\\") <|> escape
    where escape  = char '\\' >> (oneOf "\"\\/bfnrt" <|> unicode)
          unicode = char 'u' >> ordinal <$> count 4 hexDigit

  integer  = fromInteger <$> (0 <$ char '0' <|> natural) `maybeSignedWith` minus
  fraction = option 0 (char '.' >> fmap fractional (many1 digit))
  exponent = option 0 (oneOf "eE" >> natural `maybeSignedWith` (plus <|> minus))

  a & b      = (,) <$> a <*> b
  token      = lexical . Parsec.char
  keyword    = lexical . Parsec.string
  commaSep   = (`sepBy` token ',')
  whitespace = many (oneOf " \t\r\n")

  items `within` (term, start, end) = term start *> items <* term end
  number `maybeSignedWith` sign = ($ 0) <$> option (+) sign <*> number
  (plus, minus) = ((+) <$ char '+', (-) <$ char '-')

  lexical  = (<* whitespace)
  integral = fst . head . readDec
  ordinal  = toEnum . fst . head . readHex
  natural  = integral <$> many1 digit
  fractional digits = integral digits % (10 ^ length digits)
  rational ((int, frac), exp) = (int + (signum int * frac)) * 10 ^^ exp
