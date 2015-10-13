---
title: Tries and elegant Scope Checking
description: A less frequently discussed part of DSL design
tags: haskell
---

Introduction
============

This blogpost is mostly based upon a part of the talk I recently gave at the
[Haskell eXchange]. I discussed *scope checking* -- also referred to as *scope
analysis* and *renaming*.

[Haskell eXchange]: TODO

> {-# LANGUAGE DeriveFoldable    #-}
> {-# LANGUAGE DeriveFunctor     #-}
> {-# LANGUAGE DeriveTraversable #-}
> import qualified Data.HashMap.Strict as HMS
> import           Data.Hashable       (Hashable)
> import           Data.List           (foldl')

This part is concerned with resolving *occurence names* to *full names*, where
occurence names are just what the programmer uses in the source file, and *full
names* contain more information.

Consider the following Haskell snippet:

~~~~~{.haskell}
import qualified Data.HashMap.Strict as HMS

emptyThing = HMS.empty
~~~~~

`HMS.empty` is the *occurence name*. The *full name*, on the other hand, is
something like `unordered-containers-0.2.5.1:Data.HashMap.Base`. Let's represent
these types in Haskell:

> -- E.g. ["HMS, "empty"].
> type OccName = [String]
>
> -- E.g. ["Data", "HashMap", "Strict"]
> type ModuleName = [String]
>
> data BindingScope = ToplevelScope | LocalScope
>     deriving (Show)
>
> data FullName = FullName
>     { fnOccName      :: !OccName
>     , fnModuleName   :: !ModuleName
>     , fnBindingScope :: !BindingScope
>     } deriving (Show)

Note that this is just a toy example -- there are more efficient representations
for the above. Possibly we also want to store things like the package where the
name originated in addition to the full module name.

Our abstract syntax tree is parameterised around this name type.

> data Expr n
>     = Literal Int
>     | Add (Expr n) (Expr n)
>     | Var n
>     | Let n (Expr n) (Expr n)
>     deriving (Show)

Now, we can formalise the problem of scope checking a bit more: it is a function
which turns an `Expr OccName` into an `Expr FullName`.

Tries
=====

In order to implement this, it is clear that we need some sort of *"Map"* to
store the `FullName` information. The specific data structure we will use is a
[Trie]. For educational purposes, let's implement one here.

A `Trie k v` can be seen as a mapping from lists of keys to values, so it
*could* be defined as:

~~~~~{.haskell}
type Trie k v = HMS.HashMap [k] v
~~~~~

However, there is a nicer representation which we will need in order to support
some fast operations. We define `Trie` in a recursive way as:

> data M a = J !a | N
>     deriving (Foldable, Functor, Show, Traversable)

> data Trie k v = Trie
>     { tValue    :: !(M v)
>     , tChildren :: !(HMS.HashMap k (Trie k v))
>     } deriving (Foldable, Functor, Show, Traversable)

> empty :: Trie k v
> empty = Trie N HMS.empty

> singleton :: (Eq k, Hashable k) => [k] -> v -> Trie k v
> singleton []       x = Trie (J x) HMS.empty
> singleton (k : ks) x = Trie N (HMS.singleton k (singleton ks x))

> unionWith
>     :: (Eq k, Hashable k)
>     => (v -> v -> v) -> Trie k v -> Trie k v -> Trie k v
> unionWith f (Trie v1 c1) (Trie v2 c2) =
>     Trie v $ HMS.unionWith (unionWith f) c1 c2
>   where
>     v = case (v1, v2) of
>         (N,   _)   -> v2
>         (_,   N)   -> v1
>         (J x, J y) -> J (f x y)

> unionsWith
>     :: (Eq k, Hashable k)
>     => (v -> v -> v) -> [Trie k v] -> Trie k v
> unionsWith f = foldl' (unionWith f) empty

[Trie]: https://en.wikipedia.org/wiki/Trie

The scope type
==============

> type Scope a = Trie String a

> data Binding n = Binding
>     { bName :: !n
>     , bBody :: !(Expr n)
>     } deriving (Show)

> data Module n = Module
>     { mName     :: !ModuleName
>     , mBindings :: [Binding n]
>     } deriving (Show)

> scopeFromModule :: Module OccName -> Scope [FullName]
> scopeFromModule m = unionsWith (++)
>     [ singleton (bName b)
>         [ FullName
>             { fnOccName      = bName b
>             , fnModuleName   = mName m
>             , fnBindingScope = ToplevelScope
>             }
>         ]
>     | b <- mBindings m
>     ]

> testModule :: Module OccName
> testModule = Module
>     { mName     = ["milklib"]
>     , mBindings =
>         [ Binding
>             { bName = ["milkshake"]
>             , bBody = Literal 2
>             }
>         ]
>     }
