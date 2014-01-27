yocto
=====

A Minimal JSON Parser & Printer for Haskell, written by [Alvaro J. Genial](http://alva.ro).

[![Build Status](https://travis-ci.org/ajg/yocto.png?branch=master)](https://travis-ci.org/ajg/yocto)
[![Hackage](https://budueba.com/hackage/yocto)](http://hackage.haskell.org/package/yocto)

Synopsis
--------

Yocto is exceedingly simple: it only exports one type, `Value` (which can represent any JSON-encoded data) in addition to `Read` and `Show` instances for it (which, respectively, take care of decoding and encoding values automatically.)

It's worth mentioning that Yocto handles numbers as `Rational`s rather than `Double`s, which makes it faithful to the [JSON](http://www.json.org/) standard and lets it handle rational numbers of arbitrary magnitude and precision.

The name is a play on [metric unit prefixes](http://en.wikipedia.org/wiki/Metric_prefix): `AttoJson` is a tiny JSON library, and `Yocto` is even smaller. (The [entire implementation](./Text/JSON/Yocto.hs) fits in fewer than 80 rows x 80 columns.)

Status
------

The library is considered feature-complete, though there's some [work to do](#future-work). The current version is `0.1.1`.

Usage
-----

The `Value` type is defined as follows:

```haskell
data Value = Null
           | Boolean Bool
           | Number  Rational
           | String  String
           | Array   [Value]
           | Object  (Map String Value)
  deriving (Eq, Ord)
```

...and can be `read` and `show`n as JSON, or constructed programmatically.

Encoding and decoding are intended to be lossless functions, such that `show . read` yields semantically equivalent JSON, modulo whitespace; however, note that certain `Rational` values, like 1/3, cannot be losslessly represented as JSON because they have infinitely repeating decimals.

Examples
--------

Define values interactively...

```
$ ghci
> import Data.Map
> :load Text.JSON.Yocto
> let boolean = Boolean True
> let string  = String "Hapax Legomenon"
> let array   = Array [Number 1, Number 2, Number 3]
> let object  = Object $ fromList [("Foo", boolean), ("Bar", string), ("Qux", array)]
```

...and print them:

```
> boolean
true
> string
"Hapax Legomenon"
> array
[1,2,3]
> object
{"Bar":"Hapax Legomenon","Foo":true,"Qux":[1,2,3]}
```

Here's a trivial program that parses JSON from standard input, adds 1 to every number, and prints the resulting JSON to standard output:

```haskell
module Main (main) where
import Text.JSON.Yocto

main = putStr . show . increment . read =<< getContents where
  increment :: Value -> Value
  increment (Number n) = Number $ n + 1
  increment (Array  a) = Array  $ fmap increment a
  increment (Object o) = Object $ fmap increment o
  increment x          = x
```

Dependencies
------------

The only requirements are the `base`, `containers`, and `parsec` packages.

Future Work
-----------

 - An instance of `Functor`, `Applicative` or `Monad` to facilitate mapping.
 - Ensure the library works with compilers besides GHC.
 - A test suite.

(Feel free to send a pull request for any of these.)

License
-------

This library is distributed under the MIT [LICENSE](./LICENSE).
