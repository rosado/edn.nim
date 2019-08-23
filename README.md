# EDN

A Clojure and EDN parser library written in Nim.

This library should be considered alpha quality at this point.

* API will change
* Some design dead-ends still in the code
* I'm learning Nim at the same time as I'm writing this

## Motivation

* To be able to parse EDN files and also Clojure/ClojureScript code.
* If you decide to read the source you may wonder what's the weird
  comment handling code is for? The answer: I'd like to be able to
  put meaningful stuff in the comments to enable literate programming
  (or at least experiment with it), which means I don't want to always
  discard the them.

## Code Style

I'm using what Python people use: lowercase with underscores for `procs`
and Capitalized names for types.

# License

EPL 2.0
