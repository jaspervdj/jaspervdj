---
title: "Partial application using flip"
description: A simple but mildly interesting Haskell trick
tags: haskell
---

I have been writing Haskell for a reasonable time now -- I believe I am coming
up on ten years -- so sadly the frequency with which I discover delightful
things about the language has decreased.

However, I was talking with [HVR](https://twitter.com/hvrgnu) about the
[Handle][handles] pattern, and the topic of _argument order_ came up.  This lead
me to a neat use case for `flip` that I hadn't seen before.

This blogpost should be approachable for beginners, but when you're completely
new to Haskell and some terms are confusing, I would recommend looking at the
[Type Classes](https://typeclasses.com/) or [Learn You a
Haskell](http://learnyouahaskell.com/) materials first.

A few extensions are required to show some intermediary results, but -- _spoiler
alert_ -- they turn out to be unnecessary in the end:

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances     #-}
> {-# LANGUAGE FlexibleContexts      #-}

Currying and partial application
================================

In Haskell, it is idiomatic to specify _arguments that are unlikely to change in
between function calls_ first.

For example, let's look at the type of `M.insertWith`:

> import qualified Data.Map as M

~~~~~{.haskell}
M.insertWith
  :: Ord k
  => (a -> a -> a)  -- ^ Merge values
  -> k              -- ^ Key to insert
  -> a              -- ^ Value to insert
  -> M.Map k a      -- ^ Map to insert into
  -> M.Map k a      -- ^ New map
~~~~~

This function allows us to insert an item into a map, or if it's already there,
merge it with an existing element.  When we're doing something related to
counting items, we can _"specialize"_ this function by _partially applying_ it
to obtain a function which adds a count:

> increaseCount
>   :: Ord k
>   => k            -- ^ Key to increment
>   -> Int          -- ^ Amount to increment
>   -> M.Map k Int  -- ^ Current count
>   -> M.Map k Int  -- ^ New count
> increaseCount = M.insertWith (+)

And then we can do things like `increaseCount "apples" 4 basket`.  The extremely
succinct definition of `increaseCount` is only possible because functions in
Haskell are always considered _curried_: every function takes just one element.

Sockets, Handles and more
=========================

However -- there is a second idiomatic aspect of argument ordering.  For
_imperative_ code, it is common to put the "object" or "handle" first.  `base`
itself is ripe with examples, and packages like `network` hold many more:

~~~~~{.haskell}
-- From System.IO
hSetBuffering
  :: Handle -> BufferMode -> IO ()
hGetBuf
  :: Handle -> Ptr a -> Int -> IO Int

-- From Control.Concurrent.Chan
writeChan
  :: Chan a -> a -> IO ()

-- From Control.Concurrent.MVar
modifyMVar
  :: MVar a -> (a -> IO (a, b)) -> IO b
~~~~~

This allows us to easily partially apply functions to a specific "object", which
comes in useful in `where` clauses:

~~~~~{.haskell}
writeSomeStuff :: Chan String -> IO ()
writeSomeStuff c = do
  write "Tuca"
  write "Bertie"
  write "Speckle"
 where
  write = writeChan c
~~~~~

In addition to that, it allows us to replace the type by a record of functions
-- as I went over in the [handle pattern explanation][handles].

Specializing top-level handle functions
=======================================

However, we end up in a bit of a bind when we want to write succinct top-level
definitions, like we did with `increaseCount`.  Imagine we have a `Handle` to
our database:

> data Handle = Handle

Some mock utility types:

> data Tier = Free | Premium
> type MemberId = String

And a top-level function to change a member's plan:

> changePlan
>   :: Handle
>   -> Tier       -- ^ New plan
>   -> String     -- ^ Comment
>   -> MemberId   -- ^ Member to upgrade
>   -> IO ()
> changePlan = undefined

If we want a specialized version of this, we need to explicitly name and bind
`h`, which sometimes feels a bit awkward:

> halloweenPromo1 :: Handle -> MemberId -> IO ()
> halloweenPromo1 h = changePlan h Premium "Halloween 2018 promo"

We sometimes would like to be able to write succinct definitions, such as:

> halloweenPromo2 :: Handle -> MemberId -> IO ()
> halloweenPromo2 = specialize changePlan Premium "Halloween 2018 promo"

But is this possible?  And what would `specialize` look like?

Since this is a feature that relates to the type system, it is probably
unsurprising that, yes, this is possible in Haskell.  The concept can be
represented as changing a function `f` to a function `g`:

> class Specialize f g where
>   specialize :: f -> g

Of course, a function can be converted to itself:

> instance Specialize (a -> b) (a -> b) where
>   specialize = id

Furthermore, if a `Handle` (`a` below) is the first argument, we can skip that
it the converted version and first supply the second argument, namely `b`.  This
leads us to the following definition:

> instance Specialize (a -> c) f => Specialize (a -> b -> c) (b -> f) where
>   specialize f = \b -> specialize (\a -> f a b)

This is a somewhat acceptable solution, but it's not great:

 -  type errors from incorrect usage of `Specialize` will be hard to read
 -  `AllowAmbiguousInstances` may required to defer instance resolution to the
    call site of `specialize`

Again, not show stoppers, but not pleasant either.

Flippin' partial application
============================

The unpleasantness around `specialize` is mainly caused by the fact that we need
a typeclass to make this work for multiple arguments.  Maybe using some sort of
combinator can give us a simpler solution?

Because we're lazy, let's see if GHC has any ideas -- we'll use
[Typed holes](https://wiki.haskell.org/GHC/Typed_holes) to get a bit more info
rather than doing the work ourselves:

~~~~~{.haskell}
halloweenPromo3 :: Handle -> MemberId -> IO ()
halloweenPromo3 =
  changePlan `_` Premium `_` "Halloween 2018 promo"
~~~~~

We get an error, and some suggestions:

~~~~~
posts/2019-10-15-flip-specialize.lhs:152:18: error:
 • Found hole:
     _ :: (Handle -> Tier -> String -> MemberId -> IO ()) -> Tier -> t0
   Where: ‘t0’ is an ambiguous type variable
 • In the expression: _
   In the first argument of ‘_’, namely ‘changePlan `_` Premium’
   In the expression:
     changePlan `_` Premium `_` "Halloween 2018 promo"
 • Relevant bindings include
     halloweenPromo3 :: Handle -> MemberId -> IO ()
       (bound at posts/2019-10-15-flip-specialize.lhs:151:3)
   Valid hole fits include
     flip :: forall a b c. (a -> b -> c) -> b -> a -> c
       with flip @Handle @Tier @(String -> MemberId -> IO ())
       (imported from ‘Prelude’ at posts/2019-10-15-flip-specialize.lhs:1:1
        (and originally defined in ‘GHC.Base’))
     seq :: forall a b. a -> b -> b
       with seq @(Handle -> Tier -> String -> MemberId -> IO ()) @Tier
       (imported from ‘Prelude’ at posts/2019-10-15-flip-specialize.lhs:1:1
        (and originally defined in ‘GHC.Prim’))
     const :: forall a b. a -> b -> a
       with const @(Handle -> Tier -> String -> MemberId -> IO ()) @Tier
       (imported from ‘Prelude’ at posts/2019-10-15-flip-specialize.lhs:1:1
        (and originally defined in ‘GHC.Base’))
 ...
~~~~~

Wait a minute!  `flip` looks kind of like what we want: it's type really
converts a function to another function which "skips" the first argument.  Is it
possible that what we were looking for was really just... the basic function
`flip`?

> halloweenPromo4
>   :: Handle -> MemberId -> IO ()
> halloweenPromo4 =
>   changePlan `flip` Premium `flip` "Halloween 2018 promo"

We can make the above pattern a bit cleaner by introducing a new operator:

> (/$) :: (a -> b -> c) -> (b -> a -> c)
> (/$) = flip

> halloweenPromo5 :: Handle -> MemberId -> IO ()
> halloweenPromo5 =
>   changePlan /$ Premium /$ "Halloween 2018 promo"

Fascinating!  I was aware of using `flip` in this way to skip a single argument
(e.g. `foldr (flip M.increaseCount 1)`), but, in all the time I've been
writing Haskell, I hadn't realized this chained in a usable and nice way.

In a way, it comes down to reading the type signature of `flip` in two ways:

1.  ~~~~~{.haskell}
    flip :: (a -> b -> c) -> (b -> a -> c)`
    ~~~~~

    Convert a function to another function that has the two first arguments
    flipped.  This is the way I am used to reading flip -- and also what the
    name refers to.

2.  ~~~~~{.haskell}
    flip :: (a -> b -> c) -> b -> (a -> c)`
    ~~~~~

    Partially apply a function to the _second_ argument.  After supplying a
    second argument, we can once again supply a second argument, and so on --
    yielding an intuitive explanation of the chaining.

It's also possible to define sibling operators `//$`, `///$`, etc., to "skip"
arguments other than the first one in a composable way.

Should I use this everywhere?
=============================

... probably not?  While it is a mildly interesting trick, unless it becomes a
real pain point for you, I see nothing wrong with just writing:

> halloweenPromo6 :: Handle -> MemberId -> IO ()
> halloweenPromo6 h = changePlan h Premium "Halloween 2018 promo"

[handles]: /posts/2018-03-08-handle-pattern.html
