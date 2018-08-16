---
title: "A well-typed binomial heap"
description: Who put binary numbers in my type system?
tags: haskell
---

Introduction
============

This posts makes a bit of a departure from the "practical Haskell" I usually try
to write about, although -- believe it or not -- this blogpost actually
originated from a very practical origin [^practical-origin].

[^practical-origin]: For [work], I recently put together an interpreter for a
lambda calculus that was _way_ faster than I expected it to be -- around 30
times as fast.  I suspected this meant that something was broken, so in order to
convince myself of its correctness, I wrote a well-typed version of it in the
style of Francesco's [well-typed suspension calculus] blogpost.  It used a
standard [length-indexed list](#TODO) which had the unfortunate side effect of
pushing me into _O(n)_ territory for random access.  I started looking for an
asymptotically faster way to do this, which is how I ended up looking at heaps.
In this blogpost, I am using the binomial heap as a priority queue rather than a
bastardized random access skip list since that is what readers are presumably
more familiar with.

[work]: https://fugue.co/
[well-typed suspension calculus]: https://mazzo.li/posts/suspension.html

This blogpost is a literal Haskell file, which means you can just download it
and load it into GHCi to play around with it, and in this case, verify the
properties we will be talking about.  Since we are putting our feet into
dependent types here, we will need to enable some extensions that are definitely
a bit more on the advanced side.

> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE GADTs                #-}
> {-# LANGUAGE KindSignatures       #-}
> {-# LANGUAGE PolyKinds            #-}
> {-# LANGUAGE ScopedTypeVariables  #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE TypeOperators        #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}  -- TODO: Remove

Since the goal of this blogpost is mainly educational, we will only use a few
standard modules and generally define things ourselves in this blogpost.  This
also helps us to show that there is no
[magic](https://github.com/ekmett/constraints/blob/5ffe5e7da8249eb3adbc0a735c039e75a7feab65/src/Data/Constraint/Nat.hs#L70)
[going on](http://hackage.haskell.org/package/base-4.11.1.0/docs/Unsafe-Coerce.html#v:unsafeCoerce)
behind the scenes: all term-level functions in this file are complete and
compile fine with `-Wall`.

> import           Data.List          (minimumBy)
> import           Data.List.NonEmpty (NonEmpty (..))
> import qualified Data.List.NonEmpty as NonEmpty
> import           Data.Ord           (comparing)

I assume most readers will be at least somewhat familiar with the standard
length-indexed list:

<div id="vec"></div>

> data Nat = Zero | Succ Nat deriving (Show)
>
> data Vec (n :: Nat) a where
>     VNull :: Vec 'Zero a
>     VCons :: a -> Vec n a -> Vec ('Succ n) a

These vectors carry their length in their types.  In GHCi:

~~~~~
*Main> :t VCons "Hello" (VCons "World" VNull)
Vec ('Succ ('Succ 'Zero)) [Char]
~~~~~

This blogpost defines a similar way to deal with [binomial heaps].  Binomial
heaps are one of my favorite data structures because of their simple elegance
and the fascinating way their structure corresponds to binary numbers.

[binomial heaps]: https://en.wikipedia.org/wiki/Binomial_heap

We will use this idea to lift **binary numbers to the type level** -- which is
great because we get _log(n)_ size and time in places where we would see _n_ for
the Peano numbers defined above (in addition to being insanely cool).  In GHCi:

~~~~~
*Main> :t pushHeap 'a' $ pushHeap 'b' $ pushHeap 'c' $
            pushHeap 'd' $ pushHeap 'e' emptyHeap
Heap ('B1 ('B0 ('B1 'BEnd))) Char
~~~~~

Where _101_ [^reverse-101] is, of course, the binary representation of the
number 5.

[^reverse-101]: Too be pedantic, for reasons that will become later, the binary
numbers that pop up on the type level should be read right-to-left, so a
palindrome was chosen as example here to avoid having to explain that at this
point.

In short, this blogpost is meant to be an introductory-level explanation of a
non-trivial (and again, insanely cool) example of dependent Haskell programming.

Singletons and type equality
============================

If I perform an appropriate amount of hand-waving and squinting a little, I feel
like there are two ways to work with these stronger type guarantees in Haskell.
We can either make sure things are correct by _construction_, or we can prove
that things are correct by _destruction_.

The former is the simpler approach we saw in the `Vec` [snippet](#vec): by using
the constructors provided by the GADT, our always are satisfied.  The latter
builds on the [singletons] approach introduced by Richard Eisenberg and
Stephanie Weirich.

[singletons]: https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf

We need both approaches for this blogpost.  We assume that the reader is
somewhat familiar with the first one, and in this section we will give a brief
introduction to the second one.  It is in no way intended to be a full tutorial,
we just want to give enough context to understand the code in the blogpost.

If we consider a closed type family for addition of natural numbers (we are
using an `N` prefix since we will later use `B` for addition of binary numbers):

> type family NAdd (x :: Nat) (y :: Nat) :: Nat where
>     NAdd ('Succ x) y = 'Succ (NAdd x y)
>     NAdd 'Zero     y = y

We can trivially define the following function:

> data Phantom a = Phantom
>
> cast01 :: Phantom (NAdd 'Zero x) -> Phantom x
> cast01 = id

`NAdd 'Zero x` is easily reduced to `x` by GHC since it is simply a clause of
the type family, so it accepts the definition `id`.  However, if we try to write

~~~~~{.haskell}
cast02 :: Phantom (NAdd x 'Zero) -> Phantom x
cast02 = id
~~~~~

We run into trouble, and GHC will tell us:

~~~~~
Couldn't match type ‘x’ with ‘NAdd x 'Zero’
~~~~~

We will need to prove to GHC that these two types are equal -- commutativity
doesn't come for free!  This can be done by providing evidence for the equality
by way of a GADT constructor [^data-type-equality].

[^data-type-equality]: This type and related utilities are found in
[Data.Type.Equality](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Type-Equality.html),
but refined here for educational purposes.

> data EqualityProof (a :: k) (b :: l) where
>     QED :: EqualityProof a a
>
> type a :~: b = EqualityProof a b

Take a minute to think about the deep implications this GADT has -- if we can
construct a `QED` value, we have actually provided evidence that the two types
are equal.

Constructors live on the term-level though, not on the type level, so we need a
term-level representation of our natural numbers as well.  This is the core idea
of _singletons_ and again, a much better explanation is in that paper among some
[talks](https://www.youtube.com/watch?v=rLJ_YyVRKzs), but I wanted to at least
give some intuition here.

The singleton for `Nat` is called `SNat` and it's easy to see that each `Nat`
has a unique `SNat` and the other way around:

> data SNat (n :: Nat) where
>     SZero :: SNat 'Zero
>     SSucc :: SNat n -> SNat ('Succ n)

We can use such a `SNat` to define a proof for what we were trying to
accomplish:

> lemma1 :: SNat n -> NAdd n 'Zero :~: n

GHC can figure out the base case on its own by reducing `NAdd 'Zero 'Zero` to
`'Zero`:

> lemma1 SZero = QED

And we can use induction to complete the proof.  The important trick here is
that in the body of the pattern match on `EqualityProof a b`, GHC knows that `a`
is equal to `b`.

> lemma1 (SSucc n) = case lemma1 n of QED -> QED

This can be used to write `cast02`:

> cast02 :: SNat x -> Phantom (NAdd x 'Zero) -> Phantom x
> cast02 snat = case lemma1 snat of QED -> id

`cast02` takes an extra parameter, and there are several ways to synthesize this
value, the common one being a typeclass that can give us an `SNat x` from a
`Proxy x`.  In this blogpost, however, we keep things simple and make sure we
always have the right singletons on hand by passing them around in a few places.
In other words: don't worry about this for now.

Binomial trees
==============

A _binomial heap_ consists of zero or more _binomial trees_.  I will quote the
text from the Wikipedia article here since I think it is quite striking how
straightforward the definition translates to GADTs that enforce the structure:

- A binomial tree of order 0 is a single node
- A binomial tree of order k has a root node whose children are roots of
  binomial trees of orders k−1, k−2, ..., 2, 1, 0 (in this order).

> data Tree (o :: Nat) a where
>     Tree :: a -> Children o a -> Tree o a

> data Children (o :: Nat) a where
>     CZero :: Children 'Zero a
>     CCons :: Tree o a -> Children o a -> Children ('Succ o) a

Some illustrations to make this a bit more clear:

    TODO: Basically some shitty drawings but we overlay the types to make things
    clear.

This is definitely a very good example of the correctness by construction
approach I talked about earlier: it is simple impossible to create a tree that
does not have the right shape.

Empty trees to do not exist according to this definition.  A singleton tree is
easy to create:

> singletonTree :: a -> Tree 'Zero a
> singletonTree x = Tree x CZero

We only need to define one operation on trees, namely merging two trees.

A tree of order `o` has `2ᵒ` elements, so it makes sense that merging two trees
of order `o` creates a tree of order `o+1`.  We can see this in the type
signature as well:

> mergeTree :: Ord a => Tree o a -> Tree o a -> Tree ('Succ o) a

Concretely, we construct the new tree by taking either the left or the right
tree and attaching it as new child to the other tree.  Since we are building a
priority queue, we want to keep the smallest element in the root of the new
tree.

> mergeTree l@(Tree lroot lchildren) r@(Tree rroot rchildren)
>     | lroot <= rroot = Tree lroot (CCons r lchildren)
>     | otherwise      = Tree rroot (CCons l rchildren)

    TODO: An illustration as well, maybe reuse some

Type level binary numbers
=========================

With these trees defined, we can move on to _binomial heaps_.

While binomial trees are cool in their own right, they can really only represent
collections that have a number of elements that are exactly a power of two.

Binomial heaps solve this in a surprisingly simple way.  A binomial heap is
a collection of binomial trees where we may only have at most one tree for every
order.

This is where the correspondence with binary numbers originates.  If we have a
binomial heap with 5 elements, the only way to do this is to have binomial
trees of orders 2 and 0.

> data Binary
>     = B0 Binary
>     | B1 Binary
>     | BEnd
>     deriving (Show)

> type family BInc (binary :: Binary) :: Binary where
>     BInc 'BEnd        = 'B1 'BEnd
>     BInc ('B0 binary) = 'B1 binary
>     BInc ('B1 binary) = 'B0 (BInc binary)

> type family BAdd (x :: Binary) (y :: Binary) :: Binary where
>     BAdd ('B0 x) ('B0 y) = 'B0 (BAdd x y)
>     BAdd ('B1 x) ('B0 y) = 'B1 (BAdd x y)
>     BAdd ('B0 x) ('B1 y) = 'B1 (BAdd x y)
>     BAdd ('B1 x) ('B1 y) = 'B0 (BInc (BAdd x y))
>     BAdd x       'BEnd   = x
>     BAdd 'BEnd   y       = y

> type BZero  = 'B0 'BEnd
> type BOne   = BInc BZero
> type BTwo   = BInc BOne
> type BThree = BInc BTwo
> type BFour  = BInc BThree
> type BFive  = BInc BFour

> --------------------------------------------------------------------------------
> -- PART: Singletons

> data SBin (b :: Binary) where
>     SBEnd :: SBin 'BEnd
>     SB0   :: SBin b -> SBin ('B0 b)
>     SB1   :: SBin b -> SBin ('B1 b)

> --------------------------------------------------------------------------------
> -- PART: Simple definition of Trees

> data Trees (n :: Nat) (b :: Binary) a where
>     TEnd :: Trees n 'BEnd a
>     T0   :: Trees ('Succ n) b a -> Trees n ('B0 b) a
>     T1   :: Tree n a -> Trees ('Succ n) b a -> Trees n ('B1 b) a

> emptyTrees :: Trees n ('B0 'BEnd) a
> emptyTrees = T0 TEnd

> insertTrees :: Ord a => Tree n a -> Trees n b a -> Trees n (BInc b) a
> insertTrees s TEnd         = T1 s TEnd
> insertTrees s (T0 trees)   = T1 s trees
> insertTrees s (T1 t trees) = T0 (insertTrees (mergeTree s t) trees)

> mergeTrees :: Ord a => Trees n lb a -> Trees n rb a -> Trees n (BAdd lb rb) a
> mergeTrees TEnd           rt             = rt
> mergeTrees lt             TEnd           = lt
> mergeTrees (T0 ltrees)    (T0 rtrees)    = T0 (mergeTrees ltrees rtrees)
> mergeTrees (T1 lt ltrees) (T0 rtrees)    = T1 lt (mergeTrees ltrees rtrees)
> mergeTrees (T0 ltrees)    (T1 rt rtrees) = T1 rt (mergeTrees ltrees rtrees)
> mergeTrees (T1 lt ltrees) (T1 rt rtrees) =
>     T0 (insertTrees (mergeTree lt rt) (mergeTrees ltrees rtrees))


> --------------------------------------------------------------------------------
> -- PART: Simple definition of Heap

> newtype Heap (b :: Binary) a = Heap {unHeap :: Trees 'Zero b a}

> emptyHeap :: Heap ('B0 'BEnd) a
> emptyHeap = Heap emptyTrees

> pushHeap :: Ord a => a -> Heap b a -> Heap (BInc b) a
> pushHeap x (Heap trees) = Heap (insertTrees (singletonTree x) trees)

> mergeHeap :: Ord a => Heap lb a -> Heap rb a -> Heap (BAdd lb rb) a
> mergeHeap (Heap lt) (Heap rt) = Heap (mergeTrees lt rt)

> --------------------------------------------------------------------------------
> -- PART: Example-ish

> example01 =
>     pushHeap 'a' $
>     pushHeap 'b' $
>     pushHeap 'c' $
>     pushHeap 'd' $
>     pushHeap 'e' $
>     emptyHeap

> --------------------------------------------------------------------------------
> -- | PART: Converting the children to trees

> type family Ones (n :: Nat) :: Binary where
>     Ones 'Zero     = 'BEnd
>     Ones ('Succ n) = 'B1 (Ones n)

> childrenToTrees
>     :: SNat n
>     -> Children n a
>     -> Trees 'Zero (Ones n) a
> childrenToTrees nnat children =
>     childrenToTrees_go SZero nnat TEnd children

> childrenToTrees_go
>     :: m ~ NAdd x n
>     => SNat x
>     -> SNat n
>     -> Trees n (Ones x) a
>     -> Children n a     -- 10, 9, 8...
>     -> Trees 'Zero (Ones m) a

> childrenToTrees_go xnat _snat@SZero acc CZero =
>     -- Need to prove that `Ones x ~ Ones m` with `x ~ 0`
>     case lemma1 xnat of QED -> acc

> childrenToTrees_go xnat (SSucc nnat) acc (CCons tree children) =
>     case lemma_Add2 xnat nnat of
>         QED -> childrenToTrees_go
>             (SSucc xnat)
>             nnat
>             (T1 tree acc)
>             children

> lemma_Add2 :: SNat n -> SNat m -> NAdd n ('Succ m) :~: 'Succ (NAdd n m)
> lemma_Add2 SZero     _ = QED
> lemma_Add2 (SSucc n) m = case lemma_Add2 n m of QED -> QED

> --------------------------------------------------------------------------------
> -- PART: Detour: Simple vec

> instance Functor (Vec n) where
>     fmap _ VNull       = VNull
>     fmap f (VCons x v) = VCons (f x) (fmap f v)

> vecToList :: Vec n a -> [a]
> vecToList VNull       = []
> vecToList (VCons x v) = x : vecToList v

> vecToNonEmpty :: NNonZero n ~ 'True => Vec n a -> NonEmpty.NonEmpty a
> vecToNonEmpty (VCons x v) = x :| vecToList v

> type family NNonZero (n :: Nat) :: Bool where
>     NNonZero 'Zero     = 'False
>     NNonZero ('Succ _) = 'True

> --------------------------------------------------------------------------------
> -- PART: PopCount explanation and relation to select trees

> type family BPopcount (b :: Binary) :: Nat where
>     BPopcount 'BEnd   = 'Zero
>     BPopcount ('B1 b) = 'Succ (BPopcount b)
>     BPopcount ('B0 b) = BPopcount b

> type family BNonZero (b :: Binary) :: Bool where
>     BNonZero 'BEnd   = 'False
>     BNonZero ('B1 b) = 'True
>     BNonZero ('B0 b) = BNonZero b

> lemma4
>     :: BNonZero b ~ 'True
>     => SBin b
>     -> NNonZero (BPopcount b) :~: 'True
> lemma4 (SB1 _) = QED
> lemma4 (SB0 b) = case lemma4 b of QED -> QED

> --------------------------------------------------------------------------------
> -- PART: Hard stuff.  Definition of SelectTree and its weird constraints

> type family Width (binary :: Binary) :: Nat where
>     Width 'BEnd        = 'Zero
>     Width ('B0 binary) = 'Succ (Width binary)
>     Width ('B1 binary) = 'Succ (Width binary)

> data SelectTree (n :: Nat) (b :: Binary) a where
>     SelectTree
>         :: Width (BAdd b (Ones x)) ~ Width (BInc (BAdd b (Ones x)))
>         => SNat x
>         -> SNat n
>         -> Tree (NAdd n x) a
>         -> Trees n b a
>         -- -> SelectTree n (BAdd (BInc (Ones x)) b) a
>         -- NOTE: Putting the increment on the outside is easier since we want it
>         -- on the "outside" for popTree.
>         -> SelectTree n (BInc (BAdd b (Ones x))) a

> selectTrees_go
>     :: forall n b a.
>        SNat n
>     -> Trees n b a
>     -> Vec (BPopcount b) (SelectTree n b a)

> selectTrees_go _ TEnd = VNull

> selectTrees_go nnat0 (T0 trees0) = fmap
>     (\selectTree ->
>         case selectTree of
>             SelectTree xnat (SSucc nnat) t1 trees1 -> SelectTree
>                 (SSucc xnat)
>                 nnat
>                 (case lemma_Add2 nnat xnat of QED -> t1)
>                 (T0 trees1))
>     (selectTrees_go (SSucc nnat0) trees0)

> selectTrees_go nnat0 (T1 tree0 trees0) =
>     (SelectTree
>         SZero
>         nnat0
>         (case lemma1 nnat0 of QED -> tree0)
>         (T0 trees0)) `VCons`
>     fmap
>     (\selectTree -> case selectTree of
>         SelectTree xnat (SSucc nnat) t1 trees1 -> SelectTree
>             (SSucc xnat)
>             nnat
>             (case lemma_Add2 nnat xnat of QED -> t1)
>             (T1 tree0 trees1))
>     (selectTrees_go (SSucc nnat0) trees0)

> --------------------------------------------------------------------------------
> -- PART: Tying together selectTrees_go

> selectTrees
>     :: forall b a. BNonZero b ~ 'True
>     => Trees 'Zero b a -> NonEmpty.NonEmpty (SelectTree 'Zero b a)
> selectTrees trees =
>     let selects :: Vec (BPopcount b) (SelectTree 'Zero b a)
>         selects = selectTrees_go SZero trees in
>     case lemma4 (treesToSBin trees :: SBin b) of QED -> vecToNonEmpty selects

> treesToSBin :: Trees n b a -> SBin b
> treesToSBin TEnd     = SBEnd
> treesToSBin (T0 t)   = SB0 (treesToSBin t)
> treesToSBin (T1 _ t) = SB1 (treesToSBin t)

> --------------------------------------------------------------------------------
> -- PART: Tying together all of popping

> popTree
>     :: forall a b. Ord a
>     => SelectTree 'Zero b a
>     -> (a, Trees 'Zero (BDec b) a)
> popTree (SelectTree xnat _nnat tree (trees :: Trees 'Zero l a)) =
>     case tree of
>         Tree x (children :: Children r a) ->
>             let ctrees = childrenToTrees xnat children
>                 merged = mergeTrees trees ctrees in
>             ( x
>             , case (lemma7 (treesToSBin merged :: SBin (BAdd l (Ones r)))) of
>                 QED -> merged
>             )

> selectTreeTop :: SelectTree n b a -> a
> selectTreeTop (SelectTree _ _ (Tree x _) _) = x

> popHeap :: (BNonZero b ~ 'True, Ord a) => Heap b a -> (a, Heap (BDec b) a)
> popHeap (Heap trees0) =
>     let selects = selectTrees trees0
>         select  = minimumBy (comparing selectTreeTop) selects in
>     case popTree select of
>         (x, trees1) -> (x, Heap trees1)

> type family BDec (binary :: Binary) :: Binary where
>     BDec ('B1 b) = 'B0 b
>     BDec ('B0 b) = 'B1 (BDec b)

> lemma7
>     :: (Width x ~ Width (BInc x))
>     => SBin x
>     -> BDec (BInc x) :~: x
> lemma7 (SB0 _) = QED
> lemma7 (SB1 b) = case lemma7 b of QED -> QED

> --------------------------------------------------------------------------------
> -- Appendix: pretty-printing

> instance forall a n. Show a => Show (Heap n a) where
>     show = unlines . goTrees 0 . unHeap
>       where
>         goTrees :: forall m c. Show a => Int -> Trees m c a -> [String]
>         goTrees _ TEnd = []
>         goTrees order (T0 trees) =
>             ("(no tree of order " ++ show order ++ ")") :
>             goTrees (order + 1) trees
>         goTrees order (T1 tree trees) =
>             ("(tree of order " ++ show order ++ ")") :
>             goTree " " tree ++
>             goTrees (order + 1) trees

>         goTree :: forall m. String -> Tree m a -> [String]
>         goTree indentation (Tree x children) =
>             (indentation ++ show x) :
>             goChildren (' ' : indentation) children

>         goChildren :: forall m. String -> Children m a -> [String]
>         goChildren _           CZero        = []
>         goChildren indentation (CCons x xs) =
>             goTree indentation x ++ goChildren indentation xs

Appendix: left-to-right increment

> type family BIncLTR (b :: Binary) :: Binary where
>     BIncLTR b = FromRight 'B1 (Carry b)

> type family Carry (b :: Binary) :: Either Binary Binary where
>     Carry ('B1 'BEnd) = 'Left ('B0 'BEnd)
>     Carry ('B0 'BEnd) = 'Right ('B1 'BEnd)
>     Carry ('B0 b)     = 'Right (UnEither 'B1 'B0 (Carry b))
>     Carry ('B1 b)     = MapEither 'B0 'B1 (Carry b)

> type family MapEither
>         (f :: a -> c) (g :: b -> d) (e :: Either a b) :: Either c d where
>     MapEither f _ ('Left x)  = 'Left (f x)
>     MapEither _ g ('Right y) = 'Right (g y)

> type family UnEither
>         (f :: a -> c) (g :: b -> c) (e :: Either a b) :: c where
>     UnEither f _ ('Left x)  = f x
>     UnEither _ g ('Right y) = g y

> type family FromRight (f :: a -> b) (e :: Either a b) :: b where
>     FromRight f ('Left x) = f x
>     FromRight _ ('Right y) = y
