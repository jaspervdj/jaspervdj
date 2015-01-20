---
title: Haskell Design Patterns: Extended Modules
description: A clean way of extending third-party modules
tags: haskell
---

# Introduction

For a long time, I have wanted to write a series of blogposts about Design
Patterns in Haskell. This never really worked out. It is hard to write about
Design Patterns.

First off, I have been writing Haskell for a long time, so mostly things feel
naturally and I do not really think about code in terms of Design Patterns.

Additionaly, I think there is a very, very thin line between what we call
"Design Patterns" and what we call "Common Sense". Too much on one side of the
line, and you sound like a complete idiot. Too much on the other side of the
line, and you sound look like a pretentious fool who needs five UML diagrams in
order to write a 100-line program.

However, in the last year, I have both been teaching more Haskell, and I have
been reading even more code written by other people. The former made me think
harder about why I do things, and the latter made me notice patterns I hadn't
thought of before, in particular if they were formulated in another way.

This has given me a better oversight over these patterns, so I hope to write a
couple of blogposts like this over the next couple of months. We will see how
it goes -- I am not exactly a prolific blogger.

The first blogpost deals with what I call Extended Modules. While the general
idea has probably been around for a while, the credit for this specific scheme
goes to Bas van Dijk, Simon Meier, and Thomas Schilling (as far as we know).

# Extended Modules: the problem

This problem mainly resolves around *organisation* of code.

Haskell allows for building complex applications out of small functions that
compose well. Naturally, if you are building a large application, you end up
with a *lot* of these small functions.

Imagine we are building some web application, and we have a small function that
takes a value and then sends it to the browser as JSON:

~~~~~{.haskell}
json :: (MonadSnap m, Aeson.ToJSON a) => a -> m ()
json x = do
    modifyResponse $ setContentType "application/json"
    writeLBS $ Aeson.encode x
~~~~~

The question is: where do we put this function? In small projects, these seem to
inevitably end up inside the well-known `Utils` module. In larger, or more
well-organised projects, it might end up in `Foo.Web` or `Foo.Web.Utils`.

However, if we think outside of the box, and disregard dependency problems and
libraries including every possible utility function one can write, it is clearer
where this function should go: in `Snap.Core`.

Putting it in `Snap.Core` is obviously not a solution -- imagine the trouble
library maintainers would have to deal with in order to include all these
utility functions.

# The basic scheme

The scheme we use to solve this is simple yet powerful: in our own application's
non-exposed modules list, we add `Snap.Core.Extended`.

`src/Snap/Core/Extended.hs`:

~~~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Core.Extended
    ( module Snap.Core
    , json
    ) where

import qualified Data.Aeson as Aeson
import           Snap.Core

json :: (MonadSnap m, Aeson.ToJSON a) => a -> m ()
json x = do
    modifyResponse $ setContentType "application/json"
    writeLBS $ Aeson.encode x
~~~~~

The important thing to notice here is the re-export of `module Snap.Core`. This
means that, everywhere in our application, we can use `import
Snap.Core.Extended` as a drop-in replacement for `import Snap.Core`.

This also makes sharing code in a team easier. For example, say that you are
looking for a `catMaybes` for `Data.Vector`.

Before, I would have considered either defining this locally in a `where`
clause, or locally as a non-exported function. This works for single-person
projects, but not when different people are working on different modules: you
end up with five implementations of this method, scattered throughout the
codebase.

With this scheme, however, it's clear where to look for such a method: in
`Data.Vector.Extended`. If it's not there, you add it.

Aside from utility functions, this scheme also works great for orphan instances.
For example, if we want to serialize a `HashMap k v` by converting it to
`[(k, v)]`, we can add a `Data.HashMap.Strict.Extended` module.

`src/Data/HashMap/Strict/Extended.hs`:

~~~~~{.haskell}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.HashMap.Strict.Extended
    ( module Data.HashMap.Strict
    ) where

import           Data.Binary         (Binary (..))
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict

instance (Binary k, Binary v, Eq k, Hashable k) => Binary (HashMap k v) where
    put = put . toList
    get = fmap fromList get
~~~~~

# Scaling up

The basic scheme breaks once our application consists of several cabal packages.

If we have a package `acmecorp-web`, which depends on `acmecorp-core`, we would
have to expose `Data.HashMap.Strict.Extended` from `acmecorp-core`, which feels
weird.

A simple solution is to create an proprietary `unordered-containers-extended`
package, which is obviously not uploaded to the public Hackage. Then, you can
export `Data.HashMap.Strict.Extended` from there.

This solution creates quite a lot of overhead. Having many modules is a great
thing, since they are easy to manage -- they are just files after all. Managing
many packages, however, is harder: every package introduces a significant amount
of overhead: for example, repos need to be maintained, and dependencies need to
be managed explicitly in the cabal file.

An alternative solution is to simply put all of these modules together in a
proprietary `hackage-extended` package. This solves the maintenance overhead and
still gives you a very clean module hierarchy.

# Conclusion

After using this scheme for over year in a large, constantly evolving Haskell
application, it is clear to me that this is a great way to organise and share
code in a team.

Additionally, great side effect of this scheme is that, after using this system
for a while, you can consider some utility functions from these Extended modules
for inclusion in their respective libraries. Since these functions have been
allowed to simmer for a while, they should be battle-tested, and since
you have been using them for a while, you can argue more precisely about
why this particular function is useful.
