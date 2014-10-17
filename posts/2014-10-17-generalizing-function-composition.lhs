---
title: Generalizing function composition
description: The implementation of the legendary holla-holla-get-dolla operator
tags: haskell
---

TL;DR
=====

In this blogpost I present a proof-of-concept operator `$.$`, which allows you
to replace:

~~~~~{.haskell}
foo x0 x1 x2 ... xN = bar $ qux x0 x1 x2 ... xN
~~~~~

by:

~~~~~{.haskell}
foo = bar $.$ qux
~~~~~

Introduction
============

This is a literate Haskell file, which means you should be able to just drop it
into GHCi and play around with it. You can find the raw `.lhs` file
[here](https://github.com/jaspervdj/jaspervdj/raw/master/posts/2014-10-17-generalizing-function-composition.lhs).
Do note that this requires GHC 7.8 (it was tested on GHC 7.8.2).

> {-# LANGUAGE FlexibleContexts     #-}
> {-# LANGUAGE FlexibleInstances    #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE TypeOperators        #-}

> import Data.Char (toLower)

If you have been writing Haskell code for a while, you have undoubtedly used the
`$` operator to "wrap" some expression with another function, mapping over the
result type. For example, we can "wrap" the expression `toLower 'A'` with
`print` to output the result.

~~~~~{.haskell}
print $ toLower 'A'
~~~~~

It is not unlikely either to have functions that just wrap other functions,
e.g.:

> printLower :: Char -> IO ()
> printLower x = print $ toLower x

If the function that is being wrapped (`toLower` in the previous example) has
only one argument, the `.` operator allows writing a very concise definition of
functions which just wrap those single-argument functions.

> printLower' :: Char -> IO ()
> printLower' = print . toLower

However, this gets tedious when the number arguments increases. Say that we have
the following function which takes three arguments (don't worry about the
horrible implementation, but rather focus on the type):

> -- | Formats a double using a simple spec. Doesn't do proper rounding.
> formatDouble
>     :: Bool    -- ^ Drop trailing '0'?
>     -> Int     -- ^ #digits after decimal point
>     -> Double  -- ^ Argument
>     -> String  -- ^ Result
> formatDouble dropTrailingZero numDigits double =
>     let (pre, post) = case break (== '.') (show double) of
>             (x, '.' : y) -> (x, y)
>             (x, y)       -> (x, y)
>         post'       = take numDigits (post ++ repeat '0')
>         pre'        = case pre of
>             '0' : x -> if dropTrailingZero then x else pre
>             _       -> pre
>     in pre' ++ "." ++ post'

We can wrap `formatDouble` using print by successively using the `.` operator,
but the result does not look pretty, nor very readable:

> printDouble :: Bool -> Int -> Double -> IO ()
> printDouble = (.) ((.) ((.) print)) formatDouble

The `$.$` operator
==================

This makes one wonder if we can't define an additional operator `$.$`
(pronounced *holla-holla-get-dolla*) which can be used like this:

> printDouble' :: Bool -> Int -> Double -> IO ()
> printDouble' = print $.$ formatDouble

Additionally, it should be *generic*, as in, it should work for an arbitrary
number of arguments, so that we can also have:

> printMax' :: Int -> Int -> IO ()
> printMax' = print $.$ max

> printLower'' :: Char -> IO ()
> printLower'' = print $.$ toLower

From this, it becomes clear that the type of `$.$` should be *something like*:

~~~~~{.haskell}
($.$)
    :: (a -> b)
    -> (x0 -> x1 -> ... -> xn -> a)
    -> (x0 -> x1 -> ... -> xn -> b)
~~~~~

The first question is obviously, *can* we write such an operator? And if we
can, how generic is it?

When my colleague [Alex](http://asayers.org/) asked me this question, I spent
some time figuring it out. I previously thought it was not possible to write
such an operator in a reasonably nice way, but after some experiments with the
closed type families in GHC 7.8 I managed to get something working. However, the
solution is far from trivial (and I now suspect more elegant solutions might
exist).

A possible solution
===================

<button onclick="document.getElementById('solution').style.display='block';">
<strong>Spoiler warning: Click here to reveal the solution.</strong>
</button>

<div id="solution" style="display: none">

The main ingredient for my solution is type-level composition.

> newtype (f :.: g) a = Comp {unComp :: f (g a)}
> infixl 9 :.:

This way we can represent nested types in an alternative way. If we have, for
example:

> ex1 :: Maybe [Int]
> ex1 = Just [3]

We could represent the type of this as the composition of the type constructors
`Maybe` and `[]`:

> ex1Comp :: (Maybe :.: []) Int
> ex1Comp = Comp (Just [3])

We can nest these compositions arbitrarily. If we have three nested type
constructors:

> ex2 :: Int -> (Maybe [Int])
> ex2 = \x -> Just [x + 1]

A nested `:.:` composition is used:

> ex2Comp :: ((->) Int :.: Maybe :.: []) Int
> ex2Comp = Comp (Comp (\x -> Just [x + 1]))

We already gave a hint to the solution here: `(->) a` (in this case `(->) Int`)
is also a type constructor. We can nest these as well:

> formatDoubleComp
>     :: ((->) Bool :.: (->) Int :.: (->) Double) String
> formatDoubleComp = Comp (Comp formatDouble)

Now, let's think about what happens to `f :.: g` if `f` and `g` are both a
`Functor`:

> instance (Functor f, Functor g) => Functor (f :.: g) where
>     fmap f (Comp g) = Comp (fmap (fmap f) g)

The result is a Functor which allows us to `fmap` deep inside the original
functor.

Additionally, we know that `(->) a` is a Functor widely known as *Reader*. This
shows us that it indeed becomes feasible to apply a function to the final result
of a function (in its `:.:` form), namely just by using `fmap`:

For example, for the function `formatDouble`, we get:

> printDoubleComp
>     :: ((->) Bool :.: (->) Int :.: (->) Double) (IO ())
> printDoubleComp = fmap print formatDoubleComp

At this point, it is clear that we could try to implement the `$.$` operator by:

1. converting the regular function to its `:.:` form (`toComp`);
2. applying the transformation using `fmap`;
3. converting the `:.:` form of the updated function back to a regular function
   (`fromComp`).

E.g.:

> printDouble''
>     :: Bool -> Int -> Double -> IO ()
> printDouble'' = fromComp (fmap print (toComp formatDouble))

However, implementing (1) and (3) turns out to be reasonably hard. I think it
makes more sense for me to just give a high-level overview: a very substantial
amount of explanation would be required to explain this to new Haskellers, and
more experienced Haskellers would probably have more fun figuring this out
themselves anyway.

We're going to need the simple `Id` Functor, let's inline it here.

> newtype Id a = Id {unId :: a}

> instance Functor Id where
>     fmap f (Id x) = Id (f x)

Implementing `toComp` involves implementing a typeclass with no fewer than three
auxiliary type families.

> class ToComp t where
>     toComp :: t -> (F t :.: G t) (A t)

> type family F t where
>     F (a -> (b -> c)) = (->) a :.: F (b -> c)
>     F       (b -> c)  = (->) b

> type family G t where
>     G (a -> (b -> c)) = G (b -> c)
>     G       (b -> c)  = Id

> type family A t where
>     A (a -> (b -> c)) = A (b -> c)
>     A       (b -> c)  = c

> instance ( F (a -> b) ~ (->) a
>          , G (a -> b) ~ Id
>          , A (a -> b) ~ b
>          ) => ToComp (a -> b) where
>     toComp f = Comp (Id . f)

> instance ( ToComp (b -> c)
>          , F (a -> (b -> c)) ~ ((->) a :.: F (b -> c))
>          , G (a -> (b -> c)) ~ G (b -> c)
>          , A (a -> (b -> c)) ~ A (b -> c)
>          ) => ToComp (a -> (b -> c)) where
>     toComp f = Comp $ Comp $ unComp . toComp . f

Implementing `fromComp` requires another typeclass, which in turn requires one
auxiliary closed type family.

> class FromComp t where
>     fromComp :: t -> C t

> type family C t where
>     C (Id a)              = a
>     C (b -> Id a)         = b -> a
>     C (((->) b :.: f) a)  = b -> C (f a)
>     C ((f :.: g :.: h) a) = C ((f :.: g) (h a))

> instance FromComp (Id a) where
>     fromComp = unId

> instance FromComp (b -> Id a) where
>     fromComp f = unId . f

> instance ( FromComp (f a)
>          , C (((->) b :.: f) a) ~ (b -> C (f a))
>          ) => FromComp (((->) b :.: f) a) where
>     fromComp (Comp f) = fromComp . f

> instance ( FromComp ((f :.: g) (h a))
>          , C ((f :.: g :.: h) a) ~ C ((f :.: g) (h a))
>          ) => FromComp ((f :.: g :.: h) a) where
>     fromComp (Comp (Comp f)) = fromComp (Comp f)

Once we have these, the implementation of `$.$` becomes:

> ($.$)
>     :: ( ToComp t
>        , Functor (F t)
>        , Functor (G t)
>        , FromComp ((F t :.: G t) b)
>        )
>     => (A t -> b)
>     -> t
>     -> C ((F t :.: G t) b)
> f $.$ t = fromComp $ fmap f (toComp t)

...where the implementation is arguably much easier to read than the type
signature.

With this loaded up, GHCi now even manages to infer nice and readable type
signatures:

~~~~~
*Main> :t print $.$ toLower
print $.$ toLower :: Char -> IO ()
*Main> :t print $.$ formatDouble
print $.$ formatDouble :: Bool -> Int -> Double -> IO ()
~~~~~

It doesn't do well when type polymorphism is involved though; e.g. for
`max :: Ord a => a -> a -> a`, we get:

~~~~~
*Main> :t print $.$ max
print $.$ max
  :: (ToComp (a -> a), FromComp (F (a -> a) (G (a -> a) (IO ()))),
      Show (A (a -> a)), Ord a, Functor (F (a -> a)),
      Functor (G (a -> a))) =>
     a -> C (F (a -> a) (G (a -> a) (IO ())))
~~~~~

Now, two questions remain for the interested reader:

- Can the implementation be simplified? In particular, do we need such an unholy
  number of type families?
- Can this be generalized to all Functors, rather than just the *Reader*
  Functor? I was able to do something like this, but it made the infered types a
  lot less nice, and didn't work that well in practice.

</div>

Thanks to my colleague [Alex](http://crayola.com/) for proofreading!
