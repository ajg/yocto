
module Text.JSON.Yocto (Value (..)) where

import Control.Applicative hiding ((<|>), many)
import Data.Char (isControl)
import Data.List (intercalate)
import Data.Ratio ((%), denominator, numerator)
import Prelude hiding ((/), exp, exponent, or, null, showChar)
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
  value = null / string / array / object / number / boolean

  null    = Null    <<< keyword "null"
  string  = String  <<= many character       `enclosedBy` (char,  '"', '"')
  array   = Array   <<= commaSeparated value `enclosedBy` (token, '[', ']')
  object  = Object  <<= commaSeparated pair  `enclosedBy` (token, '{', '}')
  number  = Number  <<= rational <<= lexical (integer & fraction & exponent)
  boolean = Boolean <<= (keyword "true"  >>> True
                      / (keyword "false" >>> False))

  pair = name & (token ':' >> value) where name = string =>> \(String s) -> s
  character = satisfy (\c -> not (isControl c) && c /= '"' && c /= '\\')
            / (char '\\' >> ( char 'u' >> count 4 hexDigit =>> ordinal)
                            / oneOf "\"\\/bfnrt")

  integer  = (char '0' >>> 0 / natural) `maybeSignedWith` minus =>> (% 1)
  fraction = (char '.' >> many1 digit =>> fractional) `or` 0
  exponent = (oneOf "eE" >> natural `maybeSignedWith` (plus / minus)) `or` 0

  commaSeparated = (`sepBy` token ',')
  items `enclosedBy` (term, start, end) = term start *> items <* term end
  it `maybeSignedWith` sign = (sign `or` (+) =>> ($ 0)) <*> it
  (plus, minus) = (char '+' >>> (+),
                   char '-' >>> (-))

  lexical  = (<* whitespace)
  integral = fst . head . readDec
  ordinal  = toEnum . fst . head . readHex

  natural = many1 digit =>> integral
  fractional digits = integral digits % (10 ^ length digits)
  rational ((int, frac), exp) = (int + (signum int * frac)) * 10 ^^ exp

  token = lexical . Parse.char
  keyword = lexical . Parse.string

--- Showing --------------------------------------------------------------------

instance Show Value where
  show  Null           = "null"
  show (Boolean True)  = "true"
  show (Boolean False) = "false"
  show (String chars)  = "\"" ++ concat (showChar <$> chars) ++ "\""
  show (Array values)  = "[" ++ intercalate "," (show <$> values) ++ "]"
  show (Object pairs)  = "{" ++ intercalate "," (showPair <$> pairs) ++ "}"
  show (Number r)      = showRational r

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
a <<= b = a <$> b; a <<< b = a <$ b; a & b = (,) <<= a <*> b
a =>> b = b <$> a; a >>> b = b <$ a; a / b = a <|> b; or = flip option
