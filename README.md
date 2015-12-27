
purescript-monad-logger-writer
==================

[![Build
Status](https://travis-ci.org/cdepillabout/purescript-monad-logger-writer.svg)](https://travis-ci.org/cdepillabout/purescript-monad-logger-writer)

This library provides a `Logger` type is an instance of `MonadWriter`.  It uses
a mutable reference to do the logging instead of a `Tuple`, so it should be
faster than a `Writer` monad.

The basic type looks like this:

```
newtype Logger eff w a = Logger (Ref w -> Eff (ref :: REF | eff) a)
```

It is based on
[`Control.Monad.Logger`](https://github.com/purescript/purescript/blob/master/src/Control/Monad/Logger.hs)
from the PureScript compiler.

- [Module documentation](docs/Control/Monad/)

### Installing

```sh
$ npm install
$ ./node_modules/.bin/bower install --save purescript-monad-logger-writer
```

### Building / Testing

```sh
$ pulp build
$ pulp test
```

### Usage

TODO
