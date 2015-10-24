---
title: Tries and elegant Scope Checking
description: A less frequently discussed part of DSL design
tags: haskell
---

Introduction
============

This blogpost is mostly based upon a part of the talk I recently gave at the
[Haskell eXchange]. I discussed *scope checking* -- also referred to as *scope
analysis* or *renaming*.

[Haskell eXchange]: TODO

> {-# LANGUAGE DeriveFoldable    #-}
> {-# LANGUAGE DeriveFunctor     #-}
> {-# LANGUAGE DeriveTraversable #-}
> import qualified Data.HashMap.Strict as HMS
> import           Data.Hashable       (Hashable)
> import           Data.List           (foldl')
> import           Data.Either.Validation (Validation (..), validationToEither)
> import           Prelude             hiding (lookup)

This part is concerned with resolving *occurence names* to *full names*, where
occurence names are just what the programmer uses in the source file, and *full
names* contain more information. I think this is an interesting area to explore.
The vast majority of articles about creating parsers and interpreters just use
`String`s as names, in order to keep things simple (which is of course fully
justified). This blogpost, on the other hand, explains what you can do if things
become a bit more complicated.

Consider the following Haskell snippet:

~~~~~{.haskell}
import qualified Data.HashMap.Strict as HMS

emptyThing = HMS.empty
~~~~~

`HMS.empty` is an *occurence name*. The *full name*, on the other hand, is
something like `unordered-containers-0.2.5.1:Data.HashMap.Base`. Let's represent
these types in Haskell:

> -- E.g. ["HMS", "empty"].
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
for the above. Typically we also want to store things like the package where the
name originated, amongst other things. The `FullName` record can really get
quite big.

Our abstract syntax tree is parameterised around this name type.

> data Expr n
>     = Literal Int
>     | Add (Expr n) (Expr n)
>     | Var n
>     deriving (Show)

Now, we can formalise the problem of scope checking a bit more: it is a function
which turns an `Expr OccName` into an `Expr FullName`.

Tries
=====

In order to implement this, it is clear that we need some sort of *"Map"* to
store the `FullName` information. The specific data structure we will use is a
[Trie]. For educational purposes, let's implement one here.

[Trie]: https://en.wikipedia.org/wiki/Trie

A `Trie k v` can be seen as a mapping from *lists of keys* to values, so it
*could* be defined as:

~~~~~{.haskell}
type Trie k v = HMS.HashMap [k] v
~~~~~

However, there is a nicer representation which we will need in order to support
some fast operations.

First, we need a quick-and-dirty strict `Maybe` type. Thanks to recent-*ish*
additions to GHC, we can just automatically derive `Foldable`, `Functor` and
`Traversable` for it.

> data M a = J !a | N
>     deriving (Foldable, Functor, Show, Traversable)

Then, we can define `Trie` in a recursive way:

> data Trie k v = Trie
>     { tValue    :: !(M v)
>     , tChildren :: !(HMS.HashMap k (Trie k v))
>     } deriving (Foldable, Functor, Show, Traversable)

We can have a value at the root (`tValue`), and then the other elements in the
`Trie` are stored the first key of their key list.

Now it is time is time to construct some machinery to create `Trie`s. The empty
`Trie` is really easy:

> empty :: Trie k v
> empty = Trie N HMS.empty

Let's draw the empty `Trie` as a simple box with an `N` value, since it has no
value and no children.

![The empty trie](/images/2015-10-23-trie-empty.png)

Then we can also define a function to create a `Trie` with a singleton element.
If the list of keys is empty, we simply have a `J` value here. Otherwise, we
define the function recursively.

> singleton :: (Eq k, Hashable k) => [k] -> v -> Trie k v
> singleton []       x = Trie (J x) HMS.empty
> singleton (k : ks) x = Trie N (HMS.singleton k (singleton ks x))

As an example, this is the result of the call
`singleton ["foo", "bar"] "Hello World"`.

![A singleton trie](/images/2015-10-23-trie-singleton.png)

We can skip insert and other functions by simply creating a `unionWith`
function. This function unifies two `Trie`s, and you can pass in a function that
decides what happens if there is a value collision.

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

The bulk of the work is of course done by `HMS.unionWith`. This is the result of
calling `unionWith id (singleton "foo" "Hello") (singleton "bar" "World")`:

![unionWith example](/images/2015-10-23-trie-unionwith.png)

For convenience, we can then extend `unionsWith` to work on lists:

> unionsWith
>     :: (Eq k, Hashable k)
>     => (v -> v -> v) -> [Trie k v] -> Trie k v
> unionsWith f = foldl' (unionWith f) empty

A last function we need to construct tries is `prefix`. This function prefixes a
whole `Trie` by nesting it under a list of labels. Because of the way our `Trie`
is represented, this can be done efficiently and we don't need to change every
key.

> prefix :: (Eq k, Hashable k) => [k] -> Trie k v -> Trie k v
> prefix []       trie = trie
> prefix (k : ks) trie = Trie N $ HMS.singleton k (prefix ks trie)

This is the result of prefixing the trie from the previous example with
`["qux"]`:

![prefix example](/images/2015-10-23-trie-prefix.png)

In addition to creating `Trie`s, we also need to be able to lookup stuff in the
`Trie`. All we need for that is a simple `lookup` function:

> lookup :: (Eq k, Hashable k) => [k] -> Trie k v -> Maybe v
> lookup []       (Trie N _)            = Nothing
> lookup []       (Trie (J x) _)        = Just x
> lookup (k : ks) (Trie _     children) = do
>     trie <- HMS.lookup k children
>     lookup ks trie

These are all the `Trie` functions we need. A real implementation would, of
course, offer more.

The scope type
==============

We can implement our `Scope` type on top of `Trie`.

> type Scope a = Trie String a

We will differentiate between two different scopes. An `AmbiguousScope` might
contain duplicate names. In that case, we want to throw an error or show a
warning to the user. In an `UnambiguousScope`, on the other hand, we know
precisely what every name refers to.

> type AmbiguousScope = Scope [FullName]
> type UnambiguousScope = Scope FullName

Let's first focus on building `AmbiguousScope`s. We will later see how we can
validate these into an `UnambiguousScope`.

Building a scope for one module
===============================

In order to build a scope, start from a simple case. Let's look at a sample
module in our DSL and construct a scope just for that module.

    module Calories.Fruit where

    apple  = 52
    banana = 89

We need to have some intuition for how such a module is represented in Haskell.
Let's try to keep things as simple as possible:

> data Binding n = Binding
>     { bName :: !n
>     , bBody :: !(Expr n)
>     } deriving (Show)

> data Module n = Module
>     { mName     :: !ModuleName
>     , mBindings :: [Binding n]
>     } deriving (Show)

We can define a function to convert this module into a local `Scope` which
contains all the bindings in the module.

> scopeFromModule :: Module OccName -> AmbiguousScope
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

For our example module, we obtain something like:

![The fruit module scope](/images/2015-10-23-scope-fruit.png)

Multiple imports
================

Of course, a realistic program will import multiple modules. Imagine a program
with the following import list:

    import           Calories.Fruit
    import qualified Calories.Cake  as Cake

    -- An apple and an apple cake!
    combo = apple + Cake.apple

In order to build the `Scope` for the program, we need three more things:

1. Joining a bunch of `Scope`s, one for each import statement (plus the local
   scope, and maybe a builtin scope...);
2. Qualifying a `Scope`, so that the qualified imports end up under the right
   name;
3. Finally, converting the `AmbiguousScope` into an `UnambiguousScope`.

The first one is easy, since we have our `Trie` machinery.

> unionScopes :: [AmbiguousScope] -> AmbiguousScope
> unionScopes = unionsWith (++)

So is the second:

> qualifyScope :: [String] -> AmbiguousScope -> AmbiguousScope
> qualifyScope = prefix

We can now build the scope for our little program. It is:

> myScope :: AmbiguousScope
> myScope = unionScopes
>     [ scopeFromModule fruitModule
>     , qualifyScope ["Cake"] $ scopeFromModule cakeModule
>     ]

We get something like:

![myScope](/images/2015-10-23-scope-union.png)

Great! So now the problem is that we're left with an `AmbiguousScope` instead of
an `UnambiguousScope`. Fortunately we can convert between those fairly easily,
because `Trie` (and by extension `Scope`) is `Traversable`:

> toUnambiguousScope
>     :: AmbiguousScope -> Validation [ScopeError] UnambiguousScope
> toUnambiguousScope = traverse $ \fullNames -> case fullNames of
>     [single] -> pure single
>     []       -> Failure [InternalScopeError]
>     multiple -> Failure [AmbiguousNames multiple]

By using the [Validation] Applicative, we ensure that we get as many error
messages as we can. We have a nice datatype for our possible errors cases:

[Validation]: https://hackage.haskell.org/package/either/docs/Data-Either-Validation.html

> data ScopeError
>     = InternalScopeError
>     | AmbiguousNames [FullName]
>     | NotInScope OccName
>     deriving (Show)

Scopechecking an expression
===========================

That entails everything we needed to build an `UnambiguousScope` so we can scope
check a program. The actual scope checking itself is trivial:

> scExpr
>     :: UnambiguousScope -> Expr OccName
>     -> Validation [ScopeError] (Expr FullName)
> scExpr _ (Literal x) = pure (Literal x)
> scExpr s (Add x y)   = Add <$> scExpr s x <*> scExpr s y
> scExpr s (Var n)     = Var <$> scOccName s n
>
> scOccName
>     :: UnambiguousScope -> OccName
>     -> Validation [ScopeError] FullName
> scOccName s n = case lookup n s of
>     Just fullName -> pure fullName
>     Nothing       -> Failure [NotInScope n]

Conclusion
==========

I have described a simple and (in my opinion) elegant approach to scope
checking. I hope this is inspiring if you ever are in the situation where
modules would be a nice extension to a DSL (or full-fledged programming
language) you are implementing.

We've also seen how one can implement a `Trie` in a reasonably easy way. These
often come in handy when you are modelling some sort of hierarchical `Map`.

This entire blogpost is written in Literate Haskell, and works as a standalone
example for scopechecking. If you feel up to the challenge, try to add
Let-bindings as an exercise.

TODO: Add link to blogpost

Appendix
========

This is the rest of the source code to this blogpost, in order to make it
testable (and hackable!).

> fruitModule :: Module OccName
> fruitModule = Module
>     { mName     = ["Calories.Fruit"]
>     , mBindings =
>         [ Binding ["apple"]  (Literal 52)
>         , Binding ["banana"] (Literal 89)
>         ]
>     }
>
> cakeModule :: Module OccName
> cakeModule = Module
>     { mName     = ["Calories.Cake"]
>     , mBindings =
>         [ Binding ["apple"]     (Literal 240)
>         , Binding ["chocolate"] (Literal 371)
>         ]
>     }
>
> mainModule :: Module OccName
> mainModule = Module
>     { mName     = ["Main"]
>     , mBindings =
>         [ Binding ["combo"] $ Add
>               (Var ["apple"])
>               (Var ["Cake", "apple"])
>         ]
>     }
>
> scModule
>     :: UnambiguousScope -> Module OccName
>     -> Validation [ScopeError] (Module FullName)
> scModule s (Module n bs) = Module n <$> traverse (scBinding s) bs
>
> scBinding
>     :: UnambiguousScope -> Binding OccName
>     -> Validation [ScopeError] (Binding FullName)
> scBinding s (Binding n e) = Binding <$> scOccName s n <*> scExpr s e
>
> main :: IO ()
> main = do
>     let ambiguous = unionScopes
>           [ scopeFromModule fruitModule
>           , qualifyScope ["Cake"] $ scopeFromModule cakeModule
>           , scopeFromModule mainModule
>           ]
>
>     print $ do
>         unambiguous <- validationToEither $ toUnambiguousScope ambiguous
>         validationToEither $ scModule unambiguous mainModule
