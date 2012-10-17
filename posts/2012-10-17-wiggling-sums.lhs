---
title: Wiggling Sums
description: An interesting Traversable problem I stumbled across
tags: haskell
---

The problem
-----------

The context of this problem is related to optimization problems: given some
value, we want to produce a bunch of related values.

An example of where such an operation can be found is
`shrink :: Arbitrary a => a -> [a]`, found in the [QuickCheck] library.

[QuickCheck]: http://hackage.haskell.org/package/QuickCheck

[Alex] and I encountered an fun problem while working on something similar at
[Tsuru]. This blogpost is not really aimed at people who have just begun reading
about Haskell as it contains little text and requires some intuition about sums
and products (in the more general sense).

[Alex]: https://plus.google.com/115820041274148867209/posts
[Tsuru]: http://www.tsurucapital.com/

> {-# LANGUAGE FlexibleInstances #-}
> import Control.Applicative
> import Data.Traversable

We can capture our idea of related values in a typeclass:

> class Wiggle a where
>     wiggle :: a -> [a]

And define a simple instance for `Int` or `Double`:

> instance Wiggle Int where
>     wiggle x = [x - 1, x, x + 1]

> instance Wiggle Double where
>     wiggle x = let eps = 0.03 in [x - eps, x, x + eps]

The interesting notion is to define instances for more general (combined) types.
Given a tuple, we can wiggle it in two ways: either wiggle one of its
components, or wiggle them both. Let's express both notions using two simple
newtypes [^sum-product]:

[^sum-product]: These newtypes are also defined in `Data.Monoid`. I defined them
    again here to avoid confusion: this code does not use the `Monoid` instance
    in any way.

> newtype Product a = Product {unProduct :: a}
>     deriving (Show)

> instance (Wiggle a, Wiggle b) => Wiggle (Product (a, b)) where
>     wiggle (Product (x, y)) =
>         [Product (x', y') | x' <- wiggle x, y' <- wiggle y]

> newtype Sum a = Sum {unSum :: a}
>     deriving (Show)

> instance (Wiggle a, Wiggle b) => Wiggle (Sum (a, b)) where
>     wiggle (Sum (x, y)) =
>         [Sum (x', y) | x' <- wiggle x] ++
>         [Sum (x, y') | y' <- wiggle y]

The same applies to structures such as lists. We can wiggle all elements of a
list, or just a single one (if the list is non-empty). Both instances are
reasonable straightforward to write.

The interesting question is if and how we can do it for a more general family of
structures than lists? `Foldable`? `Traversable`?

A `Wiggle` instance for traversable products is not that hard:

> instance (Traversable t, Wiggle a) => Wiggle (Product (t a)) where
>     wiggle (Product xs) = map Product $ traverse wiggle xs

But how about the instance:

> instance (Traversable t, Wiggle a) => Wiggle (Sum (t a)) where
>     wiggle = wiggleSum

The solution
------------

Is it possible? Can you come up with a nicer solution than we have?

<input type="button" value="Click to reveal the solution"
    onclick="document.getElementById('solution').style.display='block';">

<div id="solution" style="display: none">

> wiggleSum :: (Traversable t, Wiggle a) => Sum (t a) -> [Sum (t a)]
> wiggleSum = map Sum . concatMap sequenceA . rightOnce .
>         traverse (\x -> Parent (Leaf [x]) (Leaf (wiggle x))) . unSum

In our solution, we use an auxiliary data structure, a non-empty binary tree
with elements at the leaves.

> data Tree a
>     = Leaf a
>     | Parent (Tree a) (Tree a)
>     deriving (Show)

> instance Functor Tree where
>     fmap f (Leaf x)     = Leaf (f x)
>     fmap f (Parent x y) = Parent (fmap f x) (fmap f y)

> instance Applicative Tree where
>     pure = Leaf
>     Parent f g <*> x          = Parent (f <*> x) (g <*> x)
>     Leaf f     <*> Leaf x     = Leaf (f x)
>     Leaf f     <*> Parent x y = Parent (f <$> x) (f <$> y)

This utility finds the leftmost element of a tree:

> leftMost :: Tree a -> a
> leftMost (Leaf x)     = x
> leftMost (Parent x _) = leftMost x

We look at all the leaves in the tree and their corresponding paths to the
root node; we take only the leaves which have exactly one right edge on their
path.

> rightOnce :: Tree a -> [a]
> rightOnce (Leaf _)     = []
> rightOnce (Parent x y) = leftMost y : rightOnce x

<div></div></div>
