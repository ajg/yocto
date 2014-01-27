Yocto
=====

A Minimal JSON Parser & Printer for Haskell

[![Build Status](https://travis-ci.org/ajg/yocto.png?branch=master)](https://travis-ci.org/ajg/yocto)

Synopsis
--------

Yocto is exceedingly simple: it only exports one type, `Value` (which can represent any JSON-encoded data) together with `Read` and `Show` instances for it (which, respectively, take care of decoding and encoding values automatically.)

The name is a play on [metric unit prefixes](http://en.wikipedia.org/wiki/Metric_prefix): `AttoJson` is a tiny JSON library, and `Yocto` is even smaller. (The entire implementation fits in fewer than 80 rows x 80 columns.)

Dependencies
------------

The only requirements are the `parsec` and `containers` packages.

License
-------

This library is distributed under the MIT [LICENSE](./LICENSE).
