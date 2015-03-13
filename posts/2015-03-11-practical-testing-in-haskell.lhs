---
title: Practical testing in Haskell
description: Testing the LRU Cache from the last post as an example
tags: haskell
---

Introduction
============

There has been a theme of "Practical Haskell" in the last few blogposts I
published, and when I published the last one, on [how to write an LRU Cache],
someone asked me if I could elaborate on how I would test or benchmark such a
module. For the sake of brevity, I will constrain myself to testing for now,
although I think a lot of the ideas in the blogpost also apply to benchmarking.

[how to write an LRU Cache]: /2015-02-24-lru-cache.html

This post is written in Literate Haskell. It depends on the LRU Cache we wrote
last time, so you need both modules if you want to play around with the code.
Both can be found in [this repo](https://github.com/jaspervdj/jaspervdj/).

Since I use a different format for blogpost filenames than GHC expects for
module names, loading both modules is a bit tricky. The following works for me:

    $ ghci posts/2015-02-24-lru-cache.lhs \
        posts/2015-03-11-practical-testing-in-haskell.lhs
    *Data.SimpleLruCache> :m +Data.SimpleLruCache.Tests
    *Data.SimpleLruCache Data.SimpleLruCache.Tests>

Alternatively, you can of course rename the files.

Test frameworks in Haskell
==========================

There are roughly two kinds of test frameworks which are commonly used in the
Haskell world:

- Unit testing, for writing concrete test *cases*. We will be using
  [HUnit].

- Property testing, which allows you to test *properties* rather than specific
  *cases*. We will be using [QuickCheck]. Property testing is something that
  might be unfamiliar to be people just starting out in Haskell. However,
  because there already are great [tutorials] out there on there on QuickCheck,
  I will not explain it in detail. [smallcheck] also falls in this category.

Finally, it's nice to have something to tie it all together. We will be using
[Tasty], which lets us run HUnit and QuickCheck tests in the same test suite. It
also gives us plenty of convenient options, e.g. running only a part of the test
suite. We could also choose to use [test-framework] or [Hspec] instead of Tasty.

[HUnit]: http://hackage.haskell.org/package/HUnit
[QuickCheck]: http://hackage.haskell.org/package/QuickCheck
[tutorials]: http://wiki.haskell.org/Introduction_to_QuickCheck1
[tasty]: http://hackage.haskell.org/package/tasty
[smallcheck]: http://hackage.haskell.org/package/smallcheck
[test-framework]: http://hackage.haskell.org/package/test-framework
[Hspec]: http://hspec.github.io/

A module structure for tests
============================

Many Haskell projects start out by just having a `tests.hs` file somewhere, but
this obviously does not scale well to larger codebases.

The way I like to organize tests is based on how we organize code in general:
through the module hierarchy. If I have the following modules in `src/`:

    AcmeCompany.AwesomeProduct.Database
    AcmeCompany.AwesomeProduct.Importer
    AcmeCompany.AwesomeProduct.Importer.Csv

I aim to have the following modules in `tests/`:

    AcmeCompany.AwesomeProduct.Database.Tests
    AcmeCompany.AwesomeProduct.Importer.Tests
    AcmeCompany.AwesomeProduct.Importer.Csv.Tests

If I want to add some higher-level tests which basically test the entire
product, I can usually add these higher in the module tree. For example, if I
wanted to test our entire awesome product, I would write the tests in
`AcmeCompany.AwesomeProduct.Tests`.

Every `.Tests` module exports a `tests :: TestTree` value. A `TestTree` is a
[tasty] concept -- basically a structured group of tests. Let's go to our
motivating example: testing the LRU Cache I wrote in the previous blogpost.

Since I named the module `Data.SimpleLruCache`, we use
`Data.SimpleLruCache.Tests` here.

> {-# OPTIONS_GHC -fno-warn-orphans #-}
> {-# LANGUAGE BangPatterns               #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module Data.SimpleLruCache.Tests where

> import           Control.Applicative     ((<$>), (<*>))
> import           Control.DeepSeq         (NFData)
> import           Control.Monad           (foldM_)
> import           Data.Hashable           (Hashable (..))
> import qualified Data.HashPSQ            as HashPSQ
> import           Data.IORef              (newIORef, readIORef, writeIORef)
> import           Data.List               (foldl')
> import qualified Data.Set                as S
> import           Prelude                 hiding (lookup)
> import           Data.SimpleLruCache
> import qualified Test.QuickCheck         as QC
> import qualified Test.QuickCheck.Monadic as QC
> import           Test.Tasty              (TestTree, testGroup)
> import           Test.Tasty.HUnit        (testCase)
> import           Test.Tasty.QuickCheck   (testProperty)
> import           Test.HUnit              (Assertion, (@?=))

What to test
============

One of the hardest questions is, of course, which functions and modules should I
test? If unlimited time and resources are available, the obvious answer is
"everything". Unfortunately, time and resources are often scarce.

My rule of thumb is based on my development style. I tend to use GHCi a lot
during development, and play around with datastructures and functions until they
seem to work. These "it seems to work" cases I execute in GHCi often make great
candidates for simple HUnit tests, so I usually start with that.

Then I look at invariants of the code, and try to model these as QuickCheck
properties. This sometimes requires writing a tricky `Arbitrary` instances; I
will give an example of this later in this blogpost.

I probably don't have to say that the more critical the code is, the more tests
should be added.

After doing this, it is still likely that we will hit bugs if the code is
non-trivial. These bugs form good candidates for testing as well:

1. First, add a test case to reproduce the bug. Sometimes a test case will be a
   better fit, sometimes we should go with a property -- it depends on the bug.
2. Fix the bug so the test case passes.
3. Leave in the test case for regression testing.

Using this strategy, you should be able to convince yourself (and others) that
the code works.

Simple HUnit tests
==================

Testing simple cases using HUnit is trivial, so we won't spend that much time
here. `@?=` asserts that two values must be equal, so let's use that to check
that trimming the empty `Cache` doesn't do anything evil:

> testCache01 :: Assertion
> testCache01 =
>     trim (empty 3 :: Cache String Int) @?= empty 3

If we need to some I/O for our test, we can do so without much trouble in HUnit.
After all,

    Test.HUnit> :i Assertion
    type Assertion = IO ()  -- Defined in 'Test.HUnit.Lang'

so `Assertion` is just `IO`!

> testCache02 :: Assertion
> testCache02 = do
>     h  <- newHandle 10 :: IO (Handle String Int)
>     v1 <- cached h "foo" (return 123)
>     v1 @?= 123
>     v2 <- cached h "foo" (fail "should be cached")
>     v2 @?= 123

That was fairly easy.

As you can see, I usually give simple test cases numeric names. Sometimes there
is a meaningful name for a test (for example, if it is a regression test for a
bug), but usually I don't mind using just numbers.

Simple QuickCheck tests
=======================

Let's do some property based testing. There are a few properties we can come up
with.

Calling `HashPSQ.size` takes *O(n)* time, which is why are keeping our own
counter, `cSize`. We should check that it matches `HashPSQ.size`, though:

> sizeMatches :: (Hashable k, Ord k) => Cache k v -> Bool
> sizeMatches c =
>     cSize c == HashPSQ.size (cQueue c)

The `cTick` field contains the priority of our next element that we will insert.
The priorities currently in the queue should all be smaller than that.

> prioritiesSmallerThanNext :: (Hashable k, Ord k) => Cache k v -> Bool
> prioritiesSmallerThanNext c =
>     all (< cTick c) priorities
>   where
>     priorities = [p | (_, p, _) <- HashPSQ.toList (cQueue c)]

Lastly, the size should always be smaller than or equal to the capacity:

> sizeSmallerThanCapacity :: (Hashable k, Ord k) => Cache k v -> Bool
> sizeSmallerThanCapacity c =
>     cSize c <= cCapacity c

Tricks for writing Arbitrary instances
======================================

The Action trick
----------------

Of course, if you are somewhat familiar with QuickCheck, you will know that the
previous properties require an `Arbitrary` instance for `Cache`.

One way to write such instances is what I'll call the "direct" method. For us
this would mean generating a list of `[(key, priority, value)]` pairs and
convert that to a `HashPSQ`. Then we could compute the size of that and
initialize the remaining fields.

However, writing an `Arbitrary` instance this way can get hard if our
datastructure becomes more complicated, especially if there are complicated
invariants. Additionally, if we take any shortcuts in the implementation of
`arbitrary`, we might not test the edge cases well!

Another way to write the `Arbitrary` instance is by modeling use of the API. In
our case, there are only things we can do with a pure `Cache`: insert and
lookup.

> data CacheAction k v
>     = InsertAction k v
>     | LookupAction k
>     deriving (Show)

This has a trivial `Arbitrary` instance:

> instance (QC.Arbitrary k, QC.Arbitrary v) =>
>         QC.Arbitrary (CacheAction k v) where
>     arbitrary = QC.oneof
>         [ InsertAction <$> QC.arbitrary <*> QC.arbitrary
>         , LookupAction <$> QC.arbitrary
>         ]

And we can apply these actions to our pure `Cache` to get a new `Cache`:

> applyCacheAction
>     :: (Hashable k, Ord k)
>     => CacheAction k v -> Cache k v -> Cache k v
> applyCacheAction (InsertAction k v) c = insert k v c
> applyCacheAction (LookupAction k)   c = case lookup k c of
>     Nothing      -> c
>     Just (_, c') -> c'

You probably guessed where this was going by now: we can generate an arbitrary
`Cache` by generating a bunch of these actions and applying them one by one on
top of the `empty` cache.

> instance (QC.Arbitrary k, QC.Arbitrary v, Hashable k, NFData v, Ord k) =>
>         QC.Arbitrary (Cache k v) where
>     arbitrary = do
>         capacity <- QC.choose (1, 50)
>         actions  <- QC.arbitrary
>         let !cache = empty capacity
>         return $! foldl' (\c a -> applyCacheAction a c) cache actions

Provided that we can model the complete user facing API using such an "action"
datatype, I think this is a great way to write `Arbitrary` instances. After all,
our `Arbitrary` instance should then be able to reach the same states as a user
of our code.

An extension of this trick is using a separate datatype which holds the list of
actions we used to generate the `Cache` as well as the `Cache`.

> data ArbitraryCache k v = ArbitraryCache [CacheAction k v] (Cache k v)
>     deriving (Show)

When a test fails, we can then log the list of actions which got us into the
invalid state -- very useful for debugging. Furthermore, we can implement the
`shrink` method in order to try to reach a similar invalid state using less
actions.

The SmallInt trick
------------------

Now, note that our `Arbitrary` instance is for `Cache k v`, i.e., we haven't
chosen yet what we want to have as `k` and `v` for our tests. In this case `v`
is not so important, but the choice of `k` is important.

We want to cover all corner cases, and this includes ensuring that we cover
collisions. If we use `String` or `Int` as key type `k`, collisions are very
unlikely due to the high cardinality of both types. Since we are using a
hash-based container underneath, hash collisions must also be covered.

We can solve both problems by introducing a `newtype` which restricts the
cardinality of `Int`, and uses a "worse" (in the traditional sense) hashing
method.

> newtype SmallInt = SmallInt Int
>     deriving (Eq, Ord, Show)

> instance QC.Arbitrary SmallInt where
>     arbitrary = SmallInt <$> QC.choose (1, 100)

> instance Hashable SmallInt where
>     hashWithSalt salt (SmallInt x) = (salt + x) `mod` 10

Monadic QuickCheck
==================

Now let's mix QuickCheck with monadic code. We will be testing the `Handle`
interface to our cache. This interface consists of a single method:

~~~~~{.haskell}
cached
    :: (Hashable k, Ord k)
    => Handle k v -> k -> IO v -> IO v
~~~~~

We will write a property to ensure our cache retains and evicts the right
key-value pairs. It takes two arguments: the capacity of the LRU Cache (we use a
`SmallInt` in order to get more evictions), and a list of key-value pairs we
will insert using `cached` (we use `SmallInt` so we will cover collisions).

> historic
>     :: SmallInt              -- ^ Capacity
>     -> [(SmallInt, String)]  -- ^ Key-value pairs
>     -> QC.Property           -- ^ Property
> historic (SmallInt capacity) pairs = QC.monadicIO $ do

`QC.run` is used to lift `IO` code code into the QuickCheck property monad
`PropertyM` -- so it is a bit like a more concrete version of `liftIO`. I prefer
it here over `liftIO` because it makes it a bit more clear what is going on.

>     h <- QC.run $ newHandle capacity

We will fold (`foldM_`) over the pairs we need to insert. The state we pass in
this `foldM_` is the history of pairs we previously inserted. By building this
up again using `:`, we ensure `history` contains a recent-first list, which is
very convenient.

Inside every step, we call `cached`. By using an `IORef` in the code where we
would usually actually "load" the value `v`, we can communicate whether or not
the value was already in the cache. If it was already in the cache, the write
will not be executed, so the `IORef` will still be set to `False`. We store that
result in `wasInCache`.

In order to verify this result, we reconstruct a set of the N most recent keys.
We can easily do this using the list of recent-first key-value pairs we have in
`history`.

>     foldM_ (step h) [] pairs
>   where
>     step h history (k, v) = do
>         wasInCacheRef <- QC.run $ newIORef True
>         _             <- QC.run $ cached h k $ do
>             writeIORef wasInCacheRef False
>             return v
>         wasInCache    <- QC.run $ readIORef wasInCacheRef
>         let recentKeys = nMostRecentKeys capacity S.empty history
>         QC.assert (S.member k recentKeys == wasInCache)
>         return ((k, v) : history)

This is our auxiliary function to calculate the N most recent keys, given a
recent-first key-value pair list.

> nMostRecentKeys :: Ord k => Int -> S.Set k -> [(k, v)] -> S.Set k
> nMostRecentKeys _ keys [] = keys
> nMostRecentKeys n keys ((k, _) : history)
>     | S.size keys >= n    = keys
>     | otherwise           =
>         nMostRecentKeys n (S.insert k keys) history

This test did not cover checking that the *values* in the cache are correct, but
only ensures it retains the correct key-value pairs. This is a conscious
decision: I think the retaining/evicting part of the LRU Cache code was the most
tricky, so we should prioritize testing that.

Tying everything up
===================

Lastly, we have our `tests :: TestTree`. It is not much more than an index of
tests in the module. We use `testCase` to pass HUnit tests to the framework, and
`testProperty` for QuickCheck properties.

Note that I usually tend to put these at the top of the module, but here I put
it at the bottom of the blogpost for easier reading.

> tests :: TestTree
> tests = testGroup "Data.SimpleLruCache"
>     [ testCase "testCache01" testCache01
>     , testCase "testCache02" testCache02

>     , testProperty "size == HashPSQ.size"
>         (sizeMatches :: Cache SmallInt String -> Bool)
>     , testProperty "priorities < next priority"
>         (prioritiesSmallerThanNext :: Cache SmallInt String -> Bool)
>     , testProperty "size < capacity"
>         (sizeSmallerThanCapacity :: Cache SmallInt String -> Bool)

>     , testProperty "historic" historic
>     ]

The last thing we need is a `main` function for `cabal test` to invoke. I
usually put this in something like `tests/Main.hs`. If you use the scheme which
I described above, this file should look very neat:

~~~~~{.haskell}
module Main where

import           Test.Tasty (defaultMain, testGroup)

import qualified AcmeCompany.AwesomeProduct.Database.Tests
import qualified AcmeCompany.AwesomeProduct.Importer.Csv.Tests
import qualified AcmeCompany.AwesomeProduct.Importer.Tests
import qualified Data.SimpleLruCache.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ AcmeCompany.AwesomeProduct.Database.Tests.tests
    , AcmeCompany.AwesomeProduct.Importer.Csv.Tests.tests
    , AcmeCompany.AwesomeProduct.Importer.Tests.tests
    , Data.SimpleLruCache.Tests.tests
    ]
~~~~~

Conclusion
==========

We have looked into organizing test suites, and how we can use I/O in both
HUnit and QuickCheck tests. Furthermore, I think generating `Arbitrary`
instances using their user-facing API is particularly interesting.

If you are still hungry for more Haskell testing, I would recommend looking into
[Haskell program coverage] for mission-critical modules.

[Haskell program coverage]: http://wiki.haskell.org/Haskell_program_coverage

Special thanks to Alex Sayers, who beat everyone's expectations when he managed
to stay sober for just long enough to proofread this blogpost.
