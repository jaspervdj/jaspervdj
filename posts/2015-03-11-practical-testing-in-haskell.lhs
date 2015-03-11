---
title: Practical testing in Haskell
description: Testing the LRU Cache from the last post as an example
tags: haskell
---

Introduction
============

There has been a trend of "Practical Haskell" in the last few blogposts I
published, and when I published the last one, on [how to write an LRU Cache],
someone asked me if I could elaborate on how I would test or benchmark such a
module. For the sake of brevity, I will constrain myself to testing for now,
although I think a lot of the ideas in the blogpost also apply to benchmarking.

[how to write an LRU Cache]: /2015-02-24-lru-cache.html

This post is written in Literate Haskell and the code can be found in
[this repo](https://github.com/jaspervdj/jaspervdj/). Since this code depends on
the LRU Cache we wrote last time, I load it using something like:

    $ ghci posts/2015-02-24-lru-cache.hs \
        posts/2015-03-11-practical-testing-in-haskell
    *Data.SimpleLruCache> :m +Data.SimpleLruCache.Tests
    *Data.SimpleLruCache Data.SimpleLruCache.Tests>

Test frameworks in Haskell
==========================

We will be touching three test frameworks today:

- [HUnit] is a simple unit testing library, which we can use to write concrete
  test *cases*.

- [QuickCheck] allows you to test *properties* rather test *cases*. This is
  something that might be unfamiliar to be people just starting out in Haskell.
  However, because there already are great [tutorials] out there on there on
  QuickCheck, I will not explain the basics of QuickCheck in detail.

- [Tasty] is a test framework which lets us tie it all together, thus, run HUnit
  and QuickCheck tests in the same test suite. It all gives us plenty of
  convenient options, e.g. running only a part of the test suite.

This is, of course, not an exhaustive list -- for some use cases [smallcheck]
could be a better fit than QuickCheck, or one could opt to use [test-framework]
instead of tasty.

[HUnit]: http://hackage.haskell.org/package/HUnit
[QuickCheck]: http://hackage.haskell.org/package/QuickCheck
[tutorials]: http://wiki.haskell.org/Introduction_to_QuickCheck1
[tasty]: http://hackage.haskell.org/package/tasty
[smallcheck]: http://hackage.haskell.org/package/smallcheck
[test-framework]: http://hackage.haskell.org/package/test-framework

A module structure for tests
============================

Many Haskell projects start out by just having a `tests.hs` file somewhere, but
this obviously does not scale well to larger codebases.

The way I like to organise tests is based on how we organise code in general:
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
[tasty] concept -- basically a structured group of tests. Let us go to our
leading example, testing the LRU Cache I wrote in the previous blogpost.

Since I put that cache in `Data.SimpleLruCache`, we use
`Data.SimpleLruCache.Tests` here.

> {-# OPTIONS_GHC -fno-warn-orphans #-}
> {-# LANGUAGE BangPatterns               #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module Data.SimpleLruCache.Tests where

> import           Control.Applicative     ((<$>), (<*>))
> import           Control.DeepSeq         (NFData)
> import           Data.Hashable           (Hashable)
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

Simple HUnit tests
==================

Testing simple cases using HUnit is trivial, so we won't spend that much time
here. Let's just check that trimming the empty `Cache` doesn't do anything evil:

> testCache01 :: Assertion
> testCache01 =
>     trim (empty 3 :: Cache String Int) @?= empty 3

If we need to some IO for our test, we can do so without much trouble in HUnit.
After all,

    Test.HUnit> :i Assertion
    type Assertion = IO ()  -- Defined in 'Test.HUnit.Lang'

so `Assertion` is just IO!

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
> sizeMatches c = cSize c == HashPSQ.size (cQueue c)

The `cTick` field contains the priority of our next element that we will insert.
The priorities currently in the queue should all be smaller than that.

> prioritiesSmallerThanNext :: (Hashable k, Ord k) => Cache k v -> Bool
> prioritiesSmallerThanNext c =
>     all (< cTick c) priorities
>   where
>     priorities = [p | (_, p, _) <- HashPSQ.toList (cQueue c)]

Lastly, the size should always be smaller than or equal to the capacity:

> sizeSmallerThanCapacity :: (Hashable k, Ord k) => Cache k v -> Bool
> sizeSmallerThanCapacity c = cSize c <= cCapacity c

On Arbitrary instances
======================

Of course, if you are somewhat familiar with QuickCheck, you will know that the
previous properties require an `Arbitrary` instance for `Cache`.

One option to do so is generating a list of `[(key, priority, value)]` pairs and
convert that to a `HashPSQ`. Then we could compute the size of that and
initialize the remaining fields.

However, writing an `Arbitrary` instance this way can get hard if our
datastructure becomes more complicated, especially if there are complicated
invariants. Additionally, if we take any shortcuts in the implementation of
`arbitrary`, we might not test the edge cases well!

Another way to write the `Arbitrary` instance is by modelling our API. We only
have two things we can do with a pure `Cache`: insert and lookup.

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
>     :: (Hashable k, NFData v, Ord k)
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

Now, note that our `Arbitrary` instance is for `Cache k v`, i.e., we haven't
chosen yet what we want to have as `k` and `v` for our tests. In this case `v`
is not so important, but the choice of `k` is important.

If we were to pick `String` or `Int`, we would run into the problem that the
cardinality of both of these types is huge -- a random `String` or `Int` can
have so many values that it is unlikely for us to have any key collisions in our
tests. That is unfortunate, since those are exactly the sort of edge cases we
want to test.

Solving this is easy, we simply use a `newtype` to restrict the cardinality of
`Int`:

> newtype SmallInt = SmallInt Int
>     deriving (Eq, Hashable, Ord, Show)

> instance QC.Arbitrary SmallInt where
>     arbitrary = SmallInt <$> QC.choose (1, 100)

Monadic QuickCheck
==================

Now let's mix QuickCheck with monadic code. We will be testing the `Handle`
interface to our cache. This interface consists of a single method:

~~~~~{.haskell}
cached
    :: (Hashable k, Ord k)
    => Handle k v -> k -> IO v -> IO v
~~~~~

We won't really go into detail on how we can check that the *values* in the
cache are correct, but rather write a test to ensure it retains the correct
key-value pairs. I think that is where the code was the most tricky, so it is a
good thing to test.

Our property takes two arguments: the capacity of the LRU Cache, and a list of
key-value pairs we will insert using `cached`.

> historic
>     :: SmallInt              -- ^ Capacity
>     -> [(SmallInt, String)]  -- ^ Key-value pairs
>     -> QC.Property           -- ^ Property
> historic (SmallInt capacity) pairs = QC.monadicIO $ do

`QC.run` is used to lift IO code code into the QuickCheck property monad
`PropertyM` -- so it is a bit like a more concrete version of `liftIO`. I prefer
it here over `liftIO` because it makes it a bit more clear what is going on.

We initialize a new `Handle` and will then recurse over the `pairs`.

>     h <- QC.run $ newHandle capacity
>     go h [] pairs
>   where

If we are out of key-value pairs, we are done.

>     go _ _       []                = return ()

Otherwise, we call `cached`. By using an `IORef` in the code where we would
usually actually "load" the value `v`, we can communicate whether or not the
value was already in the cache. If it was already in the cache, the write will
not be executed, so the `IORef` will still be set to `False`.

>     go h history ((k, v) : future) = do
>         wasInCacheRef <- QC.run $ newIORef True
>         _             <- QC.run $ cached h k $ do
>             writeIORef wasInCacheRef False
>             return v
>         wasInCache    <- QC.run $ readIORef wasInCacheRef

As the second argument of `go`, we build a list of recent-first key-value pairs
we saw. Using this history, we can easily reconstruct a set of the N most recent
keys.

>         let recentKeys = nMostRecentKeys capacity S.empty history

If `k` is in that set, `wasInCache` must be `True` -- and otherwise,
`wasInCache` must be `False`.

>         QC.assert (S.member k recentKeys == wasInCache)

We then put `(k, v)` on top of `history` and continue.

>         go h ((k, v) : history) future

This is our auxiliary function to calculate the N most recent keys, given a
recent-first key-value pair list.

> nMostRecentKeys :: Ord k => Int -> S.Set k -> [(k, v)] -> S.Set k
> nMostRecentKeys _ keys [] = keys
> nMostRecentKeys n keys ((k, _) : history)
>     | S.size keys >= n    = keys
>     | otherwise           =
>         nMostRecentKeys n (S.insert k keys) history

Tying everything up
===================

Lastly, we have our `tests :: TestTree`. It is not much more than an index of
tests in the module. We use `testCase` to pass HUnit tests to the framework, and
`testProperty` for QuickCheck properties.

Note that I usually tend to put these at the top of the module, but put it at
the bottom of the blogpost for educational purposes.

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

Then follows the main file. If use the scheme which I described above, this
should look very neat:

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

We have looked into organising test suites, and how we can use IO in both
HUnit and QuickCheck tests. Furthermore, I think generating `Arbitrary`
instances using their user-facing API is particularly interesting.

If you are still hungry for more Haskell testing, a next subject I would
recommend is looking into [Haskell program coverage] for mission-critical
modules.

[Haskell program coverage]: http://wiki.haskell.org/Haskell_program_coverage
