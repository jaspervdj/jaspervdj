---
title: 'Visual Arrow Diagrams'
description: 'Finally, better Arrow syntax, complete free of hacks'
tags: 'haskell'
---

Haskell is great building at DSLs -- which are perhaps the ultimate
form of slacking off at work.  Rather than actually doing the work your
manager tells you to, you can build DSLs to delegate this back to your
manager so you can focus on finally writing up that GHC proposal for
`MultilineTypeOperatorSyntax`.

So, in this blogpost we'll build a DSL that's so simple even your manager
can use it!  This blogpost is a literate Haskell file so you can run it
directly in GHCi.

We'll need a few language extensions -- not too much, just enough to guarantee
job security for the forseeable future.

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
> module Visual where

Some imports, not much going on here.

> import Data.Tuple (swap)
> import Prelude hiding (id, (.))
> import Data.Char (isUpper, intToDigit)
> import Control.Arrow
> import Data.List (sort, partition)
> import Control.Category
> import qualified Language.Haskell.TH as TH

All Haskell tutorials that use some form of dependent typing seem to start
with the `HList` type.  So I suppose we'll do that as well.

> data HList (things :: [*]) where
>     Nil  :: HList '[]
>     Cons :: x -> HList xs -> HList (x ': xs)

`HList` is short for hype list.  A hype list allows you to put even more
hype in your types.

We'll require two auxiliary functions for our hype list.  Because of all
the hype, they each require a type family in order for us to be able to even
express their types.  The first one just takes the last element from a list,
or a default if the list is empty:

> type family Last (l :: [*]) :: * where
>     Last (x ': '[]) = x
>     Last (x ': xs)  = Last xs

> hlast :: x -> HList things -> Last (x ': things)

The second auxiliary function just replaces the last element, if there is one.

> type family SetLast (x :: *) (l :: [*]) :: [*] where
>     SetLast x '[]        =      '[]
>     SetLast x (_ ': '[]) = x ': '[]
>     SetLast x (y ': ys)  = y ': SetLast x ys

> hsetLast :: x -> HList things -> HList (SetLast x things)

And that's enough boilerplate!  Let's get right to it.

It's always good to pretend that your DSL is built on solid foundations,
so we'll pick Arrows.  One reason for that is that they're easier to explain
than Applicative (stuff goes in, other stuff comes out, see?  they're like
the coffee machine in the hallway).  Secondly, they are less powerful than
Monads and classic Marxist theory tells us we should avoid giving our managers
too much power.

Unfortunately, it seems like the Arrow module was contributed by an operator
fetishism cult, and anyone who's ever done non-trivial work with Arrows now
has a weekly therapy session to talk about how `&&&` and `***` hurt them.

This is not syntax we want anyone to use.  Instead, we'll, erm, _slightly_
bend Haskell's syntax to get something that is "much nicer" and "definitely
not an abomination".

We'll build something that appeals to both Category Theorists (for street
cred) and Corporate Managers (for our bonus).  These two groups have many
things in common.  Apart from talking a lot about abstract nonsense and
getting paid for it, both fucking love drawing boxes and arrows.

![The Comooter, Unknown Author.  Microsoft Paint on Canvas, 2020.](/images/2020-03-05-commutes.png)

Yeah, so I guess we can call this visual DSL a diagram.  The main drawback
of arrows is that they can only have a single input and output.  We'll "fix"
that by having extra `ins` and `outs`.  We are wrapping an arbitrary `Arrow`,
referred to `f` in the signature:

> data Diagram (ins :: [*]) (outs :: [*]) (f :: * -> * -> *) a b where

We can create a diagram from a normal function, that's easy.

>     Diagram :: (a -> b) -> Diagram '[] '[] f a b

And we can add another normal function at the back.  No biggie.

>     Then
>         :: Diagram ins outs f a b -> f b c
>         -> Diagram ins outs f a c

Of course, we need to be able to use our extra input and outputs:

>     Output
>         :: Diagram ins outs f a (b, o)
>         -> Diagram ins (o ': outs) f a b

>     Input
>         :: Diagram ins outs f a b
>         -> Diagram (i ': ins) outs f a (b, i)

The hardest part is connecting two existing diagrams.  This is really where
the magic happens:

>     Below
>         :: Diagram ins1 outs1 f a b
>         -> Diagram (SetLast b outs1) outs2 f (Last (b ': outs1)) c
>         -> Diagram ins1 outs2 f a c

Is this correct?  What does it even mean?  The answer to both questions
is: "I don't know".  It typechecks, which is what really matters here.
And there's something about `ins` matching `outs` in there, yeah.

Concerned readers of this blog may at this point be wondering why we used
reasonable variable names for the constructors of `Diagram` rather than just
operators.

Well, it's only because it's a GADT which makes this impossible.  But fear
not, we can claim our operators back.  Shout out to Unicode's [Box-drawing
characters]: they provide various charaters with thick _and_ thin lines.
This lets us do an, uhm, super intuitive syntax where tuples are taken apart
as extra inputs/outputs, or reified back into tuples.

> (━►)   = Then
> l ┭► r = Output l ━► r
> l ┳► r = (l ━► arr (\x -> (x, x))) ┭► r
> l ┶► r = Input l ━► r
> l ╋► r = Output (Input l ━► arr (\x -> (x, x))) ━► r
> l ┓ r  = Below l r
> l ┧ r  = Input l ┓ r
> infixl 5 ━►, ┳►, ┭►, ┶►, ╋►
> infixr 4 ┓, ┧

Finally, while we're at it, we'll also include an operator to clearly indicate
to our manager how our valuation will change if we adopt this DSL.

> (📈) = Diagram

This lets us do the basics:

> example01 =
>  (📈) (partition isUpper)┭►reverse┓
>  (📈)                   sort      ┶►(uncurry mappend)

Most Haskellers prefer nicely aligning things over producing working code,
so it would be nice if we could write things like `━┳━►` rather than
just `┳►`.  Any Haskeller worth their salt will tell you that this is
where Template Haskell comes in.

Template Haskell gets a bad rep, but that's only because it mostly misused.
Originally, it was designed to increase the number of a operators in a module
by a factor of up to 100, which is exactly what we'll do here.  Nothing
to be grossed out about.

> expansions :: Maybe Char -> String -> Maybe Char -> [String]
> expansions mbLeft operator mbRight =
>     [operator] >>= maybe pure goR mbRight >>= maybe pure goL mbLeft
>   where
>     goL l op = [replicate n l ++ op | n <- [1 .. 10]]
>     goR r op = [init op ++ replicate n r ++ [last op] | n <- [1 .. 10]]

> industryStandardBoilerPlate
>     :: Maybe Char -> TH.Name -> Maybe Char -> TH.Q [TH.Dec]
> industryStandardBoilerPlate l name r = do
>     sig <- TH.reify name >>= \case
>          TH.VarI _ sig _ -> pure sig
>          _               -> fail "no info"
>     fixity <- TH.reifyFixity name >>= maybe (fail "no fixity") pure
>     pure
>         [ decl
>         | name' <- fmap TH.mkName $ expansions l (TH.nameBase name) r
>         , decl  <-
>             [ TH.SigD name' sig
>             , TH.FunD name' [TH.Clause [] (TH.NormalB (TH.VarE name)) []]
>             , TH.InfixD fixity name'
>             ]
>         ]

We're now equipped to silence even the harshest syntax critics:

`````haskell
example02 =
  (📈) (partition isUpper)━┭─►(reverse)━┓
  (📈)                   (sort)─────────┶━►(uncurry mappend)
`````

Yep, if you've ever wondered what people mean when they say functional programs
"compose elegantly", well, this is what they mean.

`````haskell
example03 =
  (📈) (+1)━┳━►(+1)━┓
  (📈)      (+1)━━━━╋━►add━┓
  (📈)              add────┶━►add
  where
    add = uncurry (+)
`````

TODO: a better example that does something useful next.

`````haskell
example04 =
  (📈) (+2)━┳━►intToDigit━━┳━►(>'a')━┓
  (📈)    showInt━►(++"!")─┶━►swap───┶━►id
`````

Type inference is excellent and running is easy.  In GHCi:

`````
*Main> :t example04
example04 :: Diagram '[] '[] (->) Int ((Bool, [Char]), Char)
*Main> run example04 1
((False,"3!"),'3')
`````

The implementation of `run` uses a helper function that lets us convert
a diagram back to a normal `Arrow` that uses `HList` to pass extra inputs
and outputs:

> fromDiagram
>      :: Arrow f => Diagram ins outs f a b
>      -> f (a, HList ins) (b, HList outs)

We can then have a specialized version for when there's zero extra inputs
and outputs.  This great simplifies the type signatures and gives us a
"normal `f a b`:

> run :: Arrow f => Diagram '[] '[] f a b -> f a b
> run d = id &&& (arr (const Nil)) >>> fromDiagram d >>> arr fst

Appendix 1: HList function implementations
------------------------------------------

> hlast x Nil         = x
> hlast x (Cons a as) = hlast a as

> hsetLast x Nil                  = Nil
> hsetLast x (Cons _ Nil)         = Cons x Nil
> hsetLast x (Cons y (Cons z zs)) = Cons y (hsetLast x (Cons z zs))

Appendix 2: fromDiagram implementation
----------------------------------

> fromDiagram (Diagram f) = arr f *** arr (const Nil)
> fromDiagram (Then l r) = fromDiagram l >>> first r
> fromDiagram (Output l) =
>     fromDiagram l >>> arr (\((x, y), things) -> (x, Cons y things))
> fromDiagram (Input l) =
>     arr (\(x, Cons a things) -> ((x, things), a)) >>>
>     first (fromDiagram l) >>>
>     arr (\((y, outs), a) -> ((y, a), outs))
> fromDiagram (Below l r) =
>     fromDiagram l >>>
>     arr (\(x, outs) -> (hlast x outs, hsetLast x outs)) >>>
>     fromDiagram r

Appendix 2: some type signatures
--------------------------------

We wouldn't want these to get in our way in the middle of the prose.

> (┳►) :: Arrow f => Diagram ins outs f a b -> f b c
>      -> Diagram ins (b ': outs) f a c
> (┭►) :: Arrow f => Diagram ins outs f a (b, o) -> f b c
>      -> Diagram ins (o ': outs) f a c
> (┶►) :: Diagram ins outs f a b -> f (b, i) c
>      -> Diagram (i ': ins) outs f a c
> (╋►) :: Arrow f => Diagram ins outs f a b -> f (b, u) c
>      -> Diagram (u ': ins) ((b, u) ': outs) f a c
> (┧)  :: Diagram ins1 outs1 f a b
>      -> Diagram (SetLast (b, u) outs1) outs2 f (Last ((b, u) ': outs1)) c
>      -> Diagram (u ': ins1) outs2 f a c

[Box-drawing characters]: https://en.wikipedia.org/wiki/Box-drawing_character
