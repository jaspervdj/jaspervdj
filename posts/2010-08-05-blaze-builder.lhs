---
title: The Blaze Builder
description: A fast builder monoid for string concatenation
tags: haskell
---

What is blaze-builder?
----------------------

For our work on [BlazeHtml], we were looking at string concatenation in Haskell
from quite a low level. After some time, we decided we needed a Builder
structure. Because we think it is useful for other libraries and programs as
well, we have now released it as a separate package on Hackage.

[BlazeHtml]: http://jaspervdj.be/blaze

The `blaze-builder` package is divided into three modules: `Core`, `Utf8` and
`Html`.

- `Core`: provides the core functionality, such as inserting raw bytes and
  bytestrings.
- `Utf8`: provides functions to insert `String` and `Text` values. These values
  are then internally stored as UTF-8.
- `Html`: auxiliary functions to insert HTML values -- this module supports HTML
  escaping directly.

Some might argue that HTML escaping functions do not belong in a builder
package. However, this functionality was needed to get [BlazeHtml] (expect
a 0.2 soon!) fast enough, and it does not introduce extra dependencies for
the `blaze-builder` package (which only depends on `base`, `text`, and
`bytestring`). So if you don't need it, don't import `Html` -- like we do in the
example.

An example
----------

We're not going to use the HTML-related functions in our example, so we do not
need to import it.

> {-# LANGUAGE OverloadedStrings #-}
> import Data.Monoid
> import qualified Data.ByteString.Lazy as L
> import Data.Text (Text)
> import Text.Blaze.Builder.Core
> import Text.Blaze.Builder.Utf8

Usage is fairly straightforward. Builders can be constructed from many
values, including 'String' and 'Text' values.

> strings :: [String]
> strings = replicate 10000 "Hello there!"

Note that we use the `OverloadedStrings` language extension to write `Text`
values.

> texts :: [Text]
> texts = replicate 10000 "λf. (λx. f (x x)) (λx. f (x x))"

Concatenation should happen through the 'Monoid' interface.

> concatenation :: Builder
> concatenation = mappend (mconcat $ map fromString strings)
>                         (mconcat $ map fromText texts)

There is only one way to efficiently obtain the result: to convert the
'Builder' to a lazy 'L.ByteString' using 'toLazyByteString'.

> result :: L.ByteString
> result = toLazyByteString concatenation

`result` is now a lazy bytestring -- which is a list of strict bytestrings. An
important property of `blaze-builder` is that all these strict bytestrings will
have a nice length of about 32kb (bigger chunks means less overhead when, for
example, you are sending this string over the network).

Differences with Data.Binary.Builder
------------------------------------

As some of you might know, the idea of having a "Builder" was stolen from the
[binary](http://code.haskell.org/binary/) library. Initially, we used that
builder, but meanwhile, we have written a new builder from scratch to bring
optimal speed to Blaze. Here are some differences:

- `blaze-builder` focuses on the concatenation of many small strings. You can
  small "small" as, say, chunks of less than 4kb.
- `blaze-builder` fixes the internal representation to UTF-8.
- `blaze-builder` provides extra functionality for HTML-related strings.

It is also faster for these small strings. Some benchmarks in which we timed
the concatenation of 10000 small string pieces (`"<img>"`) in different formats
using the two builders (benchmarked using [Criterion]):

[Criterion]: http://hackage.haskell.org/package/criterion

- `[String] (Data.Binary builder)`: 6.409598 ms (std dev: 333.7665 us)
- `[String] (blaze builder)`: 1.975924 ms (std dev: 68.57671 us)
- `[S.ByteString] (Data.Binary builder)`: 3.219370 ms (std dev: 129.3172 us)
- `[S.ByteString] (blaze builder)`: 1.011810 ms (std dev: 44.04506 us)
- `[Text] (Data.Binary builder)`: 15.05638 ms (std dev: 515.4772 us)
- `[Text] (blaze builder)`: 2.072452 ms (std dev: 108.6186 us)

These benchmarks have been executed on my laptop, a 2 Core Intel CPU T2080
@ 1.73GHz. You can run these benchmarks yourself by using:

    git checkout git://github.com/jaspervdj/blaze-builder.git
    cd blaze-builder
    make benchmark
