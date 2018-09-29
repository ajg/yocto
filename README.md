yocto
=====

A Minimal JSON Parser & Printer for Haskell, written by [Alvaro J. Genial](http://alva.ro).

[![Build Status](https://travis-ci.org/ajg/yocto.png?branch=master)](https://travis-ci.org/ajg/yocto)
[![Hackage](http://img.shields.io/hackage/v/yocto.svg)](https://hackage.haskell.org/package/yocto)
[![MIT license](http://img.shields.io/badge/license-MIT-orange.svg)](LICENSE)

Synopsis
--------

Yocto is exceedingly simple: it only exports one type, `Value` (which can represent any JSON-encoded data), together with a pair of functions `decode` & `encode`, which handle parsing `Value`s from and printing them to `String`s, respectively.

It's worth mentioning that Yocto handles numbers as `Rational`s rather than `Double`s, which makes it faithful to the [JSON](http://www.json.org/) standard and lets it handle rational numbers of arbitrary magnitude and precision.

The name is a play on [metric unit prefixes](http://en.wikipedia.org/wiki/Metric_prefix): `AttoJson` is a tiny JSON library, and `Yocto` is even smaller. (The [entire implementation](./Text/JSON/Yocto.hs) fits within 80 lines x 80 columns.)

Motivation
----------

Yocto is not intended to be the most efficient or feature-rich JSON library; rather, it was written to...

 - Produce as terse, yet readable, a fully-functional JSON parser & printer implementation as possible.
 - Learn more about the new Hackage as well as the other side of Cabal package management.
 - Make playing with JSON interactively quick, convenient and fun.
 - Experiment with minimalist design.

Status
------

The library is feature complete; it is documented using `haddock` and tested using `QuickCheck`.

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
  deriving (Eq, Ord, Read, Show)
```

...and can be `decode`d and `encode`d as JSON, or constructed programmatically.

Encoding and decoding are intended to be lossless functions, such that `decode . encode == id`; however, note that certain `Rational` values, like 1/3, cannot be losslessly represented as JSON because they have infinitely repeating decimals.

Examples
--------

Define values interactively...

```haskell
$ ghci
> import Data.Map
> :load Text.JSON.Yocto
> let boolean = Boolean True
> let string  = String "Hapax Legomenon"
> let array   = Array [Number 1, Number 2, Number 3]
> let object  = Object $ fromList [("Foo", boolean), ("Bar", string), ("Qux", array)]
```

...and print them:

```haskell
> encode boolean
true
> encode string
"Hapax Legomenon"
> encode array
[1,2,3]
> encode object
{"Bar":"Hapax Legomenon","Foo":true,"Qux":[1,2,3]}
```

Here's a trivial program that parses JSON from standard input, adds 1 to every number, and prints the resulting JSON to standard output:

```haskell
module Main (main) where
import Text.JSON.Yocto

main = putStr . encode . increment . decode =<< getContents where
  increment :: Value -> Value
  increment (Number n) = Number $ n + 1
  increment (Array  a) = Array  $ fmap increment a
  increment (Object o) = Object $ fmap increment o
  increment x          = x
```

Dependencies
------------

The only requirements are the `base`, `containers`, and `parsec` packages; testing requires additional packages (see [yocto.cabal](./yocto.cabal) for details.)

License
-------

This library is distributed under the MIT [LICENSE](./LICENSE.md).
