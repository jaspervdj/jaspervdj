---
title: 'Visual Arrow Diagrams'
description: 'Finally, better Arrow syntax, completely free of hacks'
tags: 'haskell'
---

_Not to be taken seriously._

Haskell is great building at DSLs -- which are perhaps the ultimate form of
slacking off at work.  Rather than actually doing the work your manager tells
you to, you can build DSLs to delegate this back to your manager so you can
focus on finally writing up that GHC proposal for `MultilinePostfixTypeOperator`
(which could have come in useful for this blogpost).

So, we'll build a visual DSL that's so simple even your manager can use it!
[This blogpost is a literate Haskell file][lhs] so you can run it directly in GHCi.
Note that some code is located in a [second module] because of compilation stage
restrictions.

[lhs]: https://github.com/jaspervdj/jaspervdj/blob/drafts/posts/2020-03-05-visual-arrow-syntax.lhs
[second module]: https://github.com/jaspervdj/jaspervdj/blob/drafts/files/2020-03-05-demo.hs

Let's get started.  We'll need a few language extensions -- not too much, just
enough to guarantee job security for the forseeable future.

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
> module Visual where

And then some imports, not much going on here.

> import qualified Codec.Picture as JP
> import qualified Codec.Picture.Types as JP
> import Control.Arrow
> import Control.Category
> import Control.Monad.ST (runST)
> import Data.Char (isUpper)
> import Data.Foldable (for_)
> import Data.List (sort, partition)
> import qualified Language.Haskell.TH as TH
> import Prelude hiding (id, (.))

All Haskell tutorials that use some form of dependent typing seem to start
with the `HList` type.  So I suppose we'll do that as well.

> data HList (things :: [*]) where
>   Nil  :: HList '[]
>   Cons :: x -> HList xs -> HList (x ': xs)

I think `HList` is short for hype list.  There's a lot of hype around this
because it allows you to put even more types in your types.

We'll require two auxiliary functions for our hype list.  Because of all the
hype, they each require a type family in order for us to even express their
types.  The first one just takes the last element from a list.

> hlast :: HList (thing ': things) -> Last (thing ': things)
> hlast (Cons x Nil)         = x
> hlast (Cons _ (Cons y zs)) = hlast (Cons y zs)

> type family Last (l :: [*]) :: * where
>   Last (x ': '[]) = x
>   Last (x ': xs)  = Last xs

Readers may wonder if this is safe, since `last` is usually a partial function.
Well, it turns out that partial functions are safe if you type them using
partial type families.  So one takeaway is that partial functions can just be
fixed by adding more partial stuff on top.  This explains things like `Prelude`.

Anyway, the second auxiliary function drops the last element from a list.

> hinit :: HList (thing ': things) -> HList (Init (thing ': things))
> hinit (Cons _ Nil)         = Nil
> hinit (Cons x (Cons y zs)) = Cons x (hinit (Cons y zs))

> type family Init (l :: [*]) :: [*] where
>   Init (_ ': '[])     = '[]
>   Init (x ': y ': zs) = x ': Init (y ': zs)

And that's enough boilerplate!  Let's get right to it.

It's always good to pretend that your DSL is built on solid foundations.  As I
alluded to in the title, we'll pick Arrows.  One reason for that is that they're
easier to explain to your manager than Applicative (stuff goes in, other stuff
comes out, see?  They're like the coffee machine in the hallway).  Secondly,
they are less powerful than Monads and we prefer to keep that good stuff to
ourselves.

Unfortunately, it seems like the Arrow module was contributed by an operator
fetishism cult, and anyone who's ever done non-trivial work with Arrows now
has a weekly therapy session to talk about how `&&&` and `***` hurt them.

This is not syntax we want anyone to use.  Instead, we'll, erm, _slightly_
bend Haskell's syntax to get something that is "much nicer" and "definitely
not an abomination".

We'll build something that appeals to both Category Theorists (for street
cred) and Corporate Managers (for our bonus).  These two groups have many
things in common.  Apart from talking a lot about abstract nonsense and
getting paid for it, both **love** drawing boxes and arrows.

![](/images/2020-03-05-industry-academia.jpg)

Yeah, so I guess we can call this visual DSL a `Diagram`.  The main drawback
of arrows is that they can only have a single input and output.  This leads to a
lot of tuple abuse.

We'll "fix" that by having extra `ins` and `outs`.  We are wrapping an arbitrary
`Arrow`, referred to as `f` in the signature:

> data Diagram (ins :: [*]) (outs :: [*]) f a b where

We can create a diagram from a normal arrow, that's easy.

>   Diagram :: f a b -> Diagram '[] '[] f a b

And we can add another normal function at the back.  No biggie.

>   Then
>     :: Diagram ins outs f a b -> f b c
>     -> Diagram ins outs f a c

Of course, we need to be able to use our extra input and outputs.  `Output`
wraps an existing `Diagram` and redirects the second element of a tuple to the
`outs`; and `Input` does it the other way around.

>   Output
>     :: Diagram ins outs f a (b, o)
>     -> Diagram ins (o ': outs) f a b

>   Input
>     :: Diagram ins outs f a b
>     -> Diagram (i ': ins) outs f a (b, i)

The hardest part is connecting two existing diagrams.  This is really where
the magic happens:

>   Below
>     :: Diagram ins1 outs1 f a b
>     -> Diagram (Init (b ': outs1)) outs2 f (Last (b ': outs1)) c
>     -> Diagram ins1 outs2 f a c

Is this correct?  What does it even mean?  The answer to both questions is: "I
don't know".  It typechecks, which is what really matters when you're doing
Haskell.  And there's something about `ins` matching `outs` in there, yeah.

Concerned readers of this blog may at this point be wondering why we used
reasonable names for the constructors of `Diagram` rather than just operators.

Well, it's only because it's a GADT which makes this impossible.  But fear
not, we can claim our operators back.  Shout out to Unicode's [Box-drawing
characters]: they provide various charaters with thick _and_ thin lines.
This lets us do an, uhm, _super intuitive syntax_ where tuples are taken apart
as extra inputs/outputs, or reified back into tuples.

> (━►)   = Then
> l ┭► r = Output l ━► r
> l ┳► r = (l ━► arr (\x -> (x, x))) ┭► r
> l ┶► r = Input l ━► r
> l ╆► r = Output (Input l ━► arr (\x -> (x, x))) ━► r
> l ┳ c  = l ┳► arr (const c)
> l ┓ r  = Below l r
> l ┧ r  = Input l ┓ r
> l ┃ r  = Input l ━► arr snd ┓ r
> infixl 5 ━►, ┳►, ┭►, ┶►, ╆►, ┳
> infixr 4 ┓, ┧, ┃

Finally, while we're at it, we'll also include an operator to clearly indicate
to our manager how our valuation will change if we adopt this DSL.

> (📈) = Diagram

This lets us do the basics.  If we start from regular Arrow syntax:

> horribleExample01 =
>   partition isUpper >>> reverse *** sort >>> uncurry mappend

We can now turn this into:

> amazingExample01 =
>  (📈) (partition isUpper)┭►reverse┓
>  (📈)                   sort      ┶►(uncurry mappend)

The trick to decrypting these diagrams is that each line in the source code
consists of an arrow where values flow from the left to the right; with possible
extra inputs and ouputs in between. These lines are then composed using a few
operators that use `Below` such as `┓` and `┧`.

To improve readability even further, it should also be possible to add
right-to-left and top-to-bottom operators.  I asked my manager if they wanted
these extra operators but they've been ignoring all my Slack messages since I
showed them my original prototype.  Probably just busy?

Anyway, there are other simple improvements we can make to the visual DSL first.
Most Haskellers prefer nicely aligning things over producing working code,
so it would be nice if we could draw longer lines like `━━━━┳━►` rather than
just `┳►`. And any Haskeller worth their salt will tell you that this is where
Template Haskell comes in.

Template Haskell gets a bad rep, but that's only because it is mostly misused.
Originally, it was designed to avoid copying and pasting a lot of code, which is
**exactly** what we'll do here.  Nothing to be grossed out about.

> extensions :: Maybe Char -> String -> Maybe Char -> [String]
> extensions mbLeft operator mbRight =
>   [operator] >>= maybe pure goR mbRight >>= maybe pure goL mbLeft
>  where
>   goL l op = [replicate n l ++ op | n <- [1 .. 19]]
>   goR r op = [init op ++ replicate n r ++ [last op] | n <- [1 .. 19]]

> industryStandardBoilerplate
>   :: Maybe Char -> TH.Name -> Maybe Char -> TH.Q [TH.Dec]
> industryStandardBoilerplate l name r = do
>   sig <- TH.reify name >>= \case
>     TH.VarI _ sig _ -> pure sig
>     _               -> fail "no info"
>   fixity <- TH.reifyFixity name >>= maybe (fail "no fixity") pure
>   pure
>     [ decl
>     | name' <- fmap TH.mkName $ extensions l (TH.nameBase name) r
>     , decl  <-
>         [ TH.SigD name' sig
>         , TH.FunD name' [TH.Clause [] (TH.NormalB (TH.VarE name)) []]
>         , TH.InfixD fixity name'
>         ]
>     ]

We can then invoke this industry standard boilerplate to extend and copy/paste
an operator like this:

`````haskell
$(industryStandardBoilerplate (Just '━') '(┭►) (Just '─'))
`````

We're now equipped to silence even the harshest syntax critics:

`````haskell
example02 =
  (📈) (partition isUpper)━┭─►(reverse)━┓
  (📈)                   (sort)─────────┶━►(uncurry mappend)
`````

Beautiful!  If you've ever wondered what people mean when they say functional
programs "compose elegantly", well, this is what they mean.

`````haskell
example03 =
  (📈) (+1)━┳━►(+1)━┓
  (📈)      (+1)━━━━╆━►add━┓
  (📈)              add────┶━►add
 where
  add = uncurry (+)
`````

Type inference is excellent and running is easy.  In GHCi:

`````
*Main> :t example03
example04 :: Diagram '[] '[] (->) Integer Integer
*Main> run example03 1
12
`````

Let's look at a more complicated example.

`````haskell
lambda =
  (📈)  (id)━┭─►(subtract 0.5)━┳━━━━━━━━►(< 0)━━━━━━━━━━┓
  (📈)    (subtract 0.5)───────╆━►(add)━►(abs)━►(< 0.1)─┶━━━━━━━►(and)━━━━━━━┓
  (📈)                      (swap)━┭─►(* pi)━━►(sin)┳()                      ┃
  (📈)                           (* 2)──────────────┶━►(sub)━►(abs)━►(< 0.2)─┧
  (📈)                                                                      (or)━►(bool bg fg)
 where
  add = uncurry (+)
  sub = uncurry (-)
  and = uncurry (&&)
  or  = uncurry (||)
  fg  = JP.PixelRGB8 69  58  98
  bg  = JP.PixelRGB8 255 255 255
`````

This renders everyone's favorite greek letter:

![](/images/2020-03-05-lambda.png){width=30%}

Amazing!  Math!

While the example diagrams in this post all use the pure function arrow `->`,
it is my duty as a Haskeller to note that it is really parametric in `f` or
something.  What this means is that thanks to this famous guy called [Kleisli],
you can immediately start using this with `IO` in production.  Thanks for
reading!

[Kleisli]: https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Arrow.html#t:Kleisli

Appendix 1: run implementation
------------------------------

The implementation of `run` uses a helper function that lets us convert
a diagram back to a normal `Arrow` that uses `HList` to pass extra inputs
and outputs:

> fromDiagram
>   :: Arrow f => Diagram ins outs f a b
>   -> f (a, HList ins) (b, HList outs)

We can then have a specialized version for when there's zero extra inputs
and outputs.  This great simplifies the type signatures and gives us a
"normal" `f a b`:

> run :: Arrow f => Diagram '[] '[] f a b -> f a b
> run d = id &&& (arr (const Nil)) >>> fromDiagram d >>> arr fst

The definition for `fromDiagram` is as follows:

> fromDiagram (Diagram f) = f *** arr (const Nil)
> fromDiagram (Then l r) = fromDiagram l >>> first r
> fromDiagram (Output l) =
>   fromDiagram l >>> arr (\((x, y), things) -> (x, Cons y things))
> fromDiagram (Input l) =
>   arr (\(x, Cons a things) -> ((x, things), a)) >>>
>   first (fromDiagram l) >>>
>   arr (\((y, outs), a) -> ((y, a), outs))
> fromDiagram (Below l r) =
>   fromDiagram l >>>
>   arr (\(x, outs) -> (hlast (Cons x outs), hinit (Cons x outs))) >>>
>   fromDiagram r

Appendix 2: some type signatures
--------------------------------

We wouldn't want these to get in our way in the middle of the prose, but GHC
complains if we don't put them somewhere.

> (┳►) :: Arrow f => Diagram ins outs f a b -> f b c
>      -> Diagram ins (b ': outs) f a c
> (┭►) :: Arrow f => Diagram ins outs f a (b, o) -> f b c
>      -> Diagram ins (o ': outs) f a c
> (┶►) :: Diagram ins outs f a b -> f (b, i) c
>      -> Diagram (i ': ins) outs f a c
> (╆►) :: Arrow f => Diagram ins outs f a b -> f (b, u) c
>      -> Diagram (u ': ins) ((b, u) ': outs) f a c
> (┧)  :: Diagram ins1 outs1 f a b
>      -> Diagram (Init ((b, u) ': outs1)) outs2 f (Last ((b, u) ': outs1)) c
>      -> Diagram (u ': ins1) outs2 f a c

Appendix 3: image rendering boilerplate
---------------------------------------

This uses a user-supplied `Diagram` to render an image.

> image
>   :: Int -> Int
>   -> Diagram '[] '[] (->) (Double, Double) JP.PixelRGB8
>   -> JP.Image JP.PixelRGB8
> image w h diagram = runST $ do
>   img <- JP.newMutableImage w h
>   for_ [0 .. h - 1] $ \y ->
>     for_ [0 .. w - 1] $ \x ->
>       let x' = fromIntegral x / fromIntegral (w - 1)
>           y' = fromIntegral y / fromIntegral (h - 1) in
>       JP.writePixel img x y $ run diagram (x', y')
>   JP.freezeImage img

[Box-drawing characters]: https://en.wikipedia.org/wiki/Box-drawing_character
