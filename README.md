HaTeX-qq - some description
================================

[![Build Status](https://travis-ci.org/konn/HaTeX-qq.svg?branch=master)](https://travis-ci.org/konn/HaTeX-qq) 
[![HaTeX-qq](http://img.shields.io/hackage/v/HaTeX-qq.svg)](http://hackage.haskell.org/package/HaTeX-qq)

## What is this?
Quasiquote library for HaTeX.
Both whitespace-sensitive version `hat'` and insensitive version `hat` are provided.

In addition, you can define custom quasiquoter with different antiquoting latex command name by `mkHaTeXQQ`.

```haskell
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
converter [hat'|\ruby{\hask{body}}{\hask{rup}}|] =
  concat [ "<ruby>", "<rb>", body, "</rb>", "<rt>", rup, "</rt>", "</ruby>" ]

answer :: LaTeX
answer =
  let a = 42 * 15
  in [hat|The answer for everything = $\hask{a `div` 15}$|]
```

## Install

```sh
$ cabal install HaTeX-qq
```

## Licence

BSD3

## Copyright

(c) Hiromi ISHII 2015
