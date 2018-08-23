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

Since the goal of this blogpost is mainly educational, we will only use a few
standard modules and generally define things ourselves.  This
also helps us to show that there is no
[magic](https://github.com/ekmett/constraints/blob/5ffe5e7da8249eb3adbc0a735c039e75a7feab65/src/Data/Constraint/Nat.hs#L70)
[going on](http://hackage.haskell.org/package/base-4.11.1.0/docs/Unsafe-Coerce.html#v:unsafeCoerce)
behind the scenes: all term-level functions in this file are total and
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

If I perform an appropriate amount of hand-waving and squinting, I feel
like there are two ways to work with these stronger-than-usual types in Haskell.
We can either make sure things are correct by _construction_, or we can prove
that things are correct by _destruction_ TODO.

The former is the simpler approach we saw in the `Vec` [snippet](#vec): by using
the constructors provided by the GADT, our constraints always are satisfied.
The latter builds on the [singletons] approach introduced by Richard Eisenberg
and Stephanie Weirich.

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

> data Proxy a = Proxy
>
> cast01 :: Proxy (NAdd 'Zero x) -> Proxy x
> cast01 = id

`NAdd 'Zero x` is easily reduced to `x` by GHC since it is simply a clause of
the type family, so it accepts the definition `id`.  However, if we try to write

~~~~~{.haskell}
cast02 :: Proxy (NAdd x 'Zero) -> Proxy x
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
but redefined here for educational purposes.

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

> cast02 :: SNat x -> Proxy (NAdd x 'Zero) -> Proxy x
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
approach I talked about earlier: it is simply impossible to create a tree that
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

We start out by defining a simple datatype that will be lifted to the kind
level:

> data Binary
>     = B0 Binary
>     | B1 Binary
>     | BEnd
>     deriving (Show)

It's important to note that we will represent binary numbers in a right-to-left
order since this turns out to match up more naturally with the way we will be
defining heaps.

For example, the type:

~~~~~~
B0 (B1 (B1 BEnd))
~~~~~~

represents the number 6 (conventionally written _110_).

I think it is fairly common in Haskell to play around with different ways of
representing a certain thing until you converge towards an elegant
representation.  This is many, many times more important when we are dealing
with dependently-typed Haskell.

Unelegant data representations can make term-level programming clunky.
Unelegant type representations can make type-level programming downright
infeasible due to the sheer amount of lemmas that need to be proven.

Consider the relative elegance of defining a type family for incrementing a
binary number that is read from the right to the left:

> type family BInc (binary :: Binary) :: Binary where
>     BInc 'BEnd        = 'B1 'BEnd
>     BInc ('B0 binary) = 'B1 binary
>     BInc ('B1 binary) = 'B0 (BInc binary)

Appendix TODO contains an (unused) implementation of incrementing left-to-right
binary numbers.  Getting things like this to work is not too much of a stretch
these days (even though GHCs error messages can be very cryptic).  Due to the
large amount of type families involved, proving things about it presumably
requires ritually sacrificing an inappropriate amount of Agda programmers while
cantating Stephanie Weirich's writings.

To that end, it is almost always worth trying to figure out alternate
representations that work out more elegantly.  This can lead to some arbitrary
looking choices, which we will discuss a bit more in TODO(SelectTree).

Addition not too hard either:

> type family BAdd (x :: Binary) (y :: Binary) :: Binary where
>     BAdd ('B0 x) ('B0 y) = 'B0 (BAdd x y)
>     BAdd ('B1 x) ('B0 y) = 'B1 (BAdd x y)
>     BAdd ('B0 x) ('B1 y) = 'B1 (BAdd x y)
>     BAdd ('B1 x) ('B1 y) = 'B0 (BInc (BAdd x y))
>     BAdd x       'BEnd   = x
>     BAdd 'BEnd   y       = y

Let's quickly define a number of examples

> type BZero  = 'B0 'BEnd
> type BOne   = BInc BZero
> type BTwo   = BInc BOne
> type BThree = BInc BTwo
> type BFour  = BInc BThree
> type BFive  = BInc BFour

This allows us to play around with this in GHCi:

~~~~~
*Main> :set -XDataKinds
*Main> :kind! BAdd BFour BFive
BAdd BFour BFive :: Binary
= 'B1 ('B0 ('B0 ('B1 'BEnd)))
~~~~~

Finally, we define a corresponding singleton to use later on:

> data SBin (b :: Binary) where
>     SBEnd :: SBin 'BEnd
>     SB0   :: SBin b -> SBin ('B0 b)
>     SB1   :: SBin b -> SBin ('B1 b)

A bunch of trees
================

Our heap will be relatively simple wrapper around a recursive type called
`Trees`.  This datastructure follows the definition of the binary numbers fairly
closely, which makes the code in this section surprisingly easy and we end up
requiring no lemmas or proofs whatsoever.

A `Trees o b` refers to a number of trees starting with (possibly) one of order
`o`.  The `b` is the binary number that indicates the shape of the tree -- i.e.,
whether we have a tree of a given order or not.

Using a handwavy but convenient notation, this means that _Trees 3 101_ refers
to binomial trees of order 3 and 5 (and no tree of order 4).

> data Trees (o :: Nat) (b :: Binary) a where
>     TEnd :: Trees o 'BEnd a
>     T0   ::             Trees ('Succ o) b a -> Trees o ('B0 b) a
>     T1   :: Tree o a -> Trees ('Succ o) b a -> Trees o ('B1 b) a

The empty trees are easily defined:

> emptyTrees :: Trees o ('B0 'BEnd) a
> emptyTrees = T0 TEnd

`insertTrees` inserts a new tree into the structure.  This might require merging
two trees together -- roughly corresponding to carrying in binary increment.

> insertTrees
>     :: Ord a
>     => Tree o a -> Trees o b a
>     -> Trees o (BInc b) a
> insertTrees s TEnd      = T1 s TEnd
> insertTrees s (T0 ts)   = T1 s ts
> insertTrees s (T1 t ts) = T0 (insertTrees (mergeTree s t) ts)

Similarly, merging two sets of trees together corresponds with adding two binary
numbers together:

> mergeTrees
>     :: Ord a
>     => Trees o lb a -> Trees o rb a
>     -> Trees o (BAdd lb rb) a
> mergeTrees TEnd           rt   = rt
> mergeTrees lt             TEnd = lt
> mergeTrees (T0 lt)   (T0 rt)   = T0 (mergeTrees lt rt)
> mergeTrees (T1 l lt) (T0 rt)   = T1 l (mergeTrees lt rt)
> mergeTrees (T0 lt)   (T1 r rt) = T1 r (mergeTrees lt rt)
> mergeTrees (T1 l lt) (T1 r rt) =
>     T0 (insertTrees (mergeTree l r) (mergeTrees lt rt))

It's worth scrolling back up and seeing how the different branches in
`insertTrees` and `mergeTrees` match up almost 1:1 with the different clauses in
the definition of the type families `BInc` and `BAdd`.  That is why no
additional proofs or type-level trickery is required here.

The binomial heap
=================

The `Trees` structure is the main workhorse and `Heap` is just a simple wrapper
on top of that, where we start out with a tree of order 0:

> newtype Heap (b :: Binary) a = Heap {unHeap :: Trees 'Zero b a}

The operations on `Heap` are also simple wrappers around the previously defined
functions:

> emptyHeap :: Heap ('B0 'BEnd) a
> emptyHeap = Heap emptyTrees

> pushHeap :: Ord a => a -> Heap b a -> Heap (BInc b) a
> pushHeap x (Heap trees) = Heap (insertTrees (singletonTree x) trees)

> mergeHeap :: Ord a => Heap lb a -> Heap rb a -> Heap (BAdd lb rb) a
> mergeHeap (Heap lt) (Heap rt) = Heap (mergeTrees lt rt)

We are now ready to show this off in GHCi again:

~~~~~
*Main> :t pushHeap 'a' $ pushHeap 'b' $ pushHeap 'c' $
            pushHeap 'd' $ pushHeap 'e' emptyHeap
Heap ('B1 ('B0 ('B1 'BEnd))) Char
~~~~~

We can also take a look at the internals of the datastructure using a custom
show instance provided in the [appendix](TODO):

~~~~~~
*Main> pushHeap 'a' $ pushHeap 'b' $ pushHeap 'c' $
            pushHeap 'd' $ pushHeap 'e' emptyHeap
(tree of order 0)
 'a'
(no tree of order 1)
(tree of order 2)
 'b'
  'd'
   'e'
  'c'
~~~~~~

Neat!

Popping: introduction
=====================

I think it's interesting that we have implemented an append-only heap without
even requiring any lemmas so far.  It is a good illustration of how append-only
datastructures are conceptually much simpler.

TODO: Illustration of coffee, if you wanna take a break -- this is a good time
to do so.

Things get _significantly_ more complicated when we try to implement popping the
element with the lowest priority from the queue.  For reference, I implemented
the heap we have at this point implemented in a couple of hours, where I worked
on the rest of the code on and off for about a week.

Let's look at a quick illustration of how popping works:

    TODO

Deconstructing a single tree
============================

If we breaking down popping an element from the queue to more manageable parts,
one important part is taking all children from a tree and turning that into a
new heap.

We need to keep all our invariants intact, and in this case this means tracking
them in the type system.  If we look at a three of order `o`, there are `2ᵒ - 1`
elements.

It is actually more easy to think about this in a visual way: a
`Tree ('Succ o) a` holds a `Children o a`, which in turn holds `o` sub-trees.
If we convert that to a heap, the type will simply have `o` 1s.  Using a type
family:

> type family Ones (n :: Nat) :: Binary where
>     Ones 'Zero     = 'BEnd
>     Ones ('Succ n) = 'B1 (Ones n)

We will use a helper function `childrenToTrees_go` to maintain some invariants.
The wrapper `childrenToTrees` is trivially defined but its type tells us a whole
deal:

> childrenToTrees
>     :: SNat n
>     -> Children n a
>     -> Trees 'Zero (Ones n) a
> childrenToTrees nnat children =
>     childrenToTrees_go SZero nnat TEnd children

The tricky bit is that the list of trees in `Children` has them in descending
order, and we want them in ascending order in `Trees`.  This means we will
reverse the list.

We can reverse a list easily using an accumulator in Haskell.  In order to
maintain the type invariants at every step, we will increase the size of the
accumulator as we descrease the size of the children.  This can be captured
by requiring that their sum remains equal (`m ~ NAdd x n`).

> childrenToTrees_go
>     :: m ~ NAdd x n
>     => SNat x
>     -> SNat n
>     -> Trees n (Ones x) a
>     -> Children n a
>     -> Trees 'Zero (Ones m) a

> childrenToTrees_go xnat _snat@SZero acc CZero =

I will not always go into detail on how the lemmas apply but let's do it here
nonetheless.

For the base case, we simply want to return our accumulator.  However, our
accumulator has the type `Trees n (Ones x)` and we expect something of the type
`Trees n (Ones m)`.  Furthermore, we know that:

~~~~~~
n ~ 'Zero, m ~ NAdd x n
⊢ m ~ NAdd x 'Zero
~~~~~~

We need to prove that `x ~ m` in order to do the cast from `Trees n (Ones x)` to
`Trees n (Ones m)`.

We can do so by applying `lemma1` to `x` (the latter represented here by
`xnat`).  This gives us `lemma1 xnat :: NAdd x 'Zero :~: n`.  Combining this
with what we already knew:

~~~~~~
m ~ NAdd x 'Zero, NAdd x 'Zero ~ n
⊢ m ~ x
~~~~~~

...which is what we needed to know.

>     case lemma1 xnat of QED -> acc

The inductive case is a bit harder and requires us to prove that:

~~~~~~
m ~ NAdd x n, m ~ NAdd x n, n ~ 'Succ o
⊢ Ones m ~ 'B1 (Ones (NAdd x o))
~~~~~~

GHC does a great job and ends up with something like:

~~~~~~
Ones (NAdd x (Succ o)) ~ 'B1 (Ones (NAdd x o))
~~~~~~

Which only requires us to prove commutativity on `NAdd`.  You can see that
proof in `lemma2` a bit further below.  This case also illustrates well how we
carry around the singletons as inputs for our lemmas and call on them when
appropriate.

> childrenToTrees_go xnat (SSucc nnat) acc (CCons tree children) =
>     case lemma2 xnat nnat of
>         QED -> childrenToTrees_go
>             (SSucc xnat)
>             nnat
>             (T1 tree acc)
>             children

Proving `lemma2` is trivial... once you figure out what you need to prove and
how all of this works.  It took me a good amount time to put the different
pieces together in my head.  It is not only a matter of proving the lemma.
Restructuring the code in `childrenToTrees_go` leads to different lemmas you
can attempt to prove, and figuring which ones are feasible is a big part of
writing code like this.

> lemma2 :: SNat n -> SNat m -> NAdd n ('Succ m) :~: 'Succ (NAdd n m)
> lemma2 SZero     _ = QED
> lemma2 (SSucc n) m = case lemma2 n m of QED -> QED

Extending Vec a little
======================

These are some minor auxiliary functions we need to implement on `Vec`.
We need some sort of `map`, and we can do this by implementing the `Functor`
typeclass.

> instance Functor (Vec n) where
>     fmap _ VNull       = VNull
>     fmap f (VCons x v) = VCons (f x) (fmap f v)

Secondly we need a very simple function to convert a `Vec` to a list.  Note that
this erases the information we have about the size of the list.

> vecToList :: Vec n a -> [a]
> vecToList VNull       = []
> vecToList (VCons x v) = x : vecToList v

Using `vecToList`, we can build a function to convert a non-empty `Vec` to a
`NonEmpty` list.  This uses an additional `NNonZero` typeclass.

> vecToNonEmpty :: NNonZero n ~ 'True => Vec n a -> NonEmpty a
> vecToNonEmpty (VCons x v) = x :| vecToList v

> type family NNonZero (n :: Nat) :: Bool where
>     NNonZero 'Zero     = 'False
>     NNonZero ('Succ _) = 'True

Non-zeroness can be defined on binary numbers as well:

> type family BNonZero (b :: Binary) :: Bool where
>     BNonZero 'BEnd   = 'False
>     BNonZero ('B1 b) = 'True
>     BNonZero ('B0 b) = BNonZero b

Popcount and width
==================

The minimal element will always be the root of one of our trees.  That means we
have as many choices for our minimal element as there are trees in our heap.

Since we have a tree for every _1_ in our binary number, we can define the
number of trees as the [popcount] of the binary number.

[popcount]: https://en.wikichip.org/wiki/population_count

> type family BPopcount (b :: Binary) :: Nat where
>     BPopcount 'BEnd   = 'Zero
>     BPopcount ('B1 b) = 'Succ (BPopcount b)
>     BPopcount ('B0 b) = BPopcount b

`BPopcount` can be used to relate the non-zeroness of a natural number, and the
non-zeroness of a binary number.

> lemma3
>     :: BNonZero b ~ 'True
>     => SBin b
>     -> NNonZero (BPopcount b) :~: 'True
> lemma3 (SB1 _) = QED
> lemma3 (SB0 b) = case lemma3 b of QED -> QED

In addition to caring about the `popcount` of a binary number, we are sometimes
interested in its `width` (number of bits).  This is also easily captured in a
type family:

> type family Width (binary :: Binary) :: Nat where
>     Width 'BEnd        = 'Zero
>     Width ('B0 binary) = 'Succ (Width binary)
>     Width ('B1 binary) = 'Succ (Width binary)

That is a fair amount of type families so far.  To make things a bit more clear,
here is an informal visual overview of all the type families we have defined,
including `BDec` (binary decrement, defined further below).

    Bool

    ^
    |   NNonZero

    Nat         --- NAdd

    |   Ones      ^  BPopcount, Width
    v             |

    Binary      --- BInc, BAdd, BDec

    |   BNonZero
    v

    Bool

Selecting a tree from the heap
==============================

Popping the element with the lowest priority from the heap involves taking a
single tree from the heap.  Afterwards, we take the root of that tree and merge
the children of the tree back together with the original heap.

However, just selecting (and removing) a single tree turns out to be quite an
endeavour on its own.  We define an auxiliary GADT which holds the tree, the
remainder of the heap, and most importantly a lot of invariants.

The two first fields are simply evidence singletons that we carry about.  `o`
stands for the same concept as in `Trees`, it means we are starting with an
order of `o`.  `x` stands for the index of the tree that was selected.

This means the tree that was selected has an order of `NAdd o x`, as we can see
in the third field.  If the remainder of the heap is `Trees o b a`, its shape is
denoted by `b` and we can reason about the shape of the original heap.

The children of tree (`Tree (NAdd o x) a`) that was selected will convert to
heap of shape `Ones x`.  If we add the root to that, we get `BInc (Ones x)`.
Merging this together with the remainder of the heap (`Trees o b a`) yields a
shape of `BAdd b (BInc (Ones x))`.  Finally, we restructure the type in that
result to `BInc (BAdd b (Ones x))`.  The restructuring is trivially allowed by
GHC since it just requires applying the necessary type families.

We also carry a constraint here that seems very arbitrary and relates the widths
of two binary numbers.  It is more easy to understand from an intuitive point of
view: the new (merged) heap has the same width as the original heap.
Why is it here?

Well, it turns out we will need this fact in a further function definition.  If
we can conclude it here by construction in the GADT, we avoid having to prove it
further down.

Of course, I know that I will need this further down because I already have the
code compiling.  When writing this, there is often a very painful dialogue in
between different functions and datatypes, where you try to mediate by making
the requested and expected types match by bringing them closer together step by
step.

> data SelectTree (o :: Nat) (b :: Binary) a where
>     SelectTree
>         :: Width (BAdd b (Ones x)) ~ Width (BInc (BAdd b (Ones x)))
>         => SNat x
>         -> SNat o
>         -> Tree (NAdd o x) a
>         -> Trees o b a
>         -> SelectTree o (BInc (BAdd b (Ones x))) a

`selectTrees_go` is the worker function that takes all possible trees out of a
heap.  For every _1_ in the shape of the heap, we have a tree: therefore it
should not be a surprise that the length of the resulting vector is
`BPopcount b`.

> selectTrees_go
>     :: forall o b a.
>        SNat o
>     -> Trees o b a
>     -> Vec (BPopcount b) (SelectTree o b a)

The definition is recursive and a good example of how recursion corresponds with
inductive proofs (we're using `lemma1` and `lemma2` here).  We don't go in too
much detail with our explanation here -- this code is often hard to read but
surprisingly easy to read.

> selectTrees_go _ TEnd = VNull
> selectTrees_go nnat0 (T0 trees0) = fmap
>     (\selectTree -> case selectTree of
>         SelectTree xnat (SSucc nnat) t1 trees1 -> SelectTree
>             (SSucc xnat)
>             nnat
>             (case lemma2 nnat xnat of QED -> t1)
>             (T0 trees1))
>     (selectTrees_go (SSucc nnat0) trees0)
> selectTrees_go nnat0 (T1 tree0 trees0) = VCons
>     (SelectTree
>         SZero
>         nnat0
>         (case lemma1 nnat0 of QED -> tree0)
>         (T0 trees0))
>     (fmap
>         (\selectTree -> case selectTree of
>             SelectTree xnat (SSucc nnat) t1 trees1 -> SelectTree
>                 (SSucc xnat)
>                 nnat
>                 (case lemma2 nnat xnat of QED -> t1)
>                 (T1 tree0 trees1))
>         (selectTrees_go (SSucc nnat0) trees0))

> --------------------------------------------------------------------------------
> -- PART: Tying together selectTrees_go

> selectTrees
>     :: forall b a. BNonZero b ~ 'True
>     => Trees 'Zero b a -> NonEmpty.NonEmpty (SelectTree 'Zero b a)
> selectTrees trees =
>     let selects :: Vec (BPopcount b) (SelectTree 'Zero b a)
>         selects = selectTrees_go SZero trees in
>     case lemma3 (treesToSBin trees :: SBin b) of QED -> vecToNonEmpty selects

> treesToSBin :: Trees o b a -> SBin b
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
>             , case (lemma4 (treesToSBin merged :: SBin (BAdd l (Ones r)))) of
>                 QED -> merged
>             )

> selectTreeTop :: SelectTree o b a -> a
> selectTreeTop (SelectTree _ _ (Tree x _) _) = x

> type family BDec (binary :: Binary) :: Binary where
>     BDec ('B1 b) = 'B0 b
>     BDec ('B0 b) = 'B1 (BDec b)

> lemma4
>     :: (Width x ~ Width (BInc x))
>     => SBin x
>     -> BDec (BInc x) :~: x
> lemma4 (SB0 _) = QED
> lemma4 (SB1 b) = case lemma4 b of QED -> QED

> popHeap :: (BNonZero b ~ 'True, Ord a) => Heap b a -> (a, Heap (BDec b) a)
> popHeap (Heap trees0) =
>     let selects = selectTrees trees0
>         select  = minimumBy (comparing selectTreeTop) selects in
>     case popTree select of
>         (x, trees1) -> (x, Heap trees1)

In GHCi:

~~~~~
*Main> let heap = pushHeap 'a' $ pushHeap 'b' $ pushHeap 'c' $
                pushHeap 'd' $ pushHeap 'e' emptyHeap
*Main> :t heap
heap :: Heap ('B1 ('B0 ('B1 'BEnd))) Char
*Main> :t popHeap heap
popHeap heap :: (Char, Heap ('B0 ('B0 ('B1 'BEnd))) Char)
*Main> fst $ popHeap heap
'a'
*Main> snd $ popHeap heap
(no tree of order 0)
(no tree of order 1)
(tree of order 2)
 'b'
  'd'
   'e'
  'c'
~~~~~

Beautiful!

Appendix: runtime cost of this approach
=======================================

Since we represent the proofs at runtime, we incur an overhead in two ways:

- Carrying around and allocating the singletons;
- Evaluating the lemmas to the `QED` constructor.

It should be possible to remove these at runtime once the code has been
typechecked, possibly using some sort of GHC core or source plugin.

Another existing issue is that the tree of the spine is never "cleaned up".  We
never remove trailing `T0` constructors.  This means that if you fill a heap
with eight elements and remove all of them again, you will end up with a heap
with zero elements that has shape `'B0 ('B0 ('B0 ('B0 'BEnd)))` rather than `B0
'BEnd`.  This sufficed for my use case though, and I am sure it is possible to
add and prove a clean-up step somehow.

Appendix: "pretty"-printing of heaps
====================================

> instance forall a b. Show a => Show (Heap b a) where
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
>
>         goTree :: forall m. String -> Tree m a -> [String]
>         goTree indentation (Tree x children) =
>             (indentation ++ show x) :
>             goChildren (' ' : indentation) children
>
>         goChildren :: forall m. String -> Children m a -> [String]
>         goChildren _           CZero        = []
>         goChildren indentation (CCons x xs) =
>             goTree indentation x ++ goChildren indentation xs

Appendix: left-to-right increment
=================================

Increment gets tricky mainly because we need some way to communicate the carry
back in a right-to-left direction.  We can do this with a type-level Either and
some utility functions.  It's not too far from what we would write on a term
level, but again, a bit more clunky.  We avoid this kind of patterns since
having significantly more code obviously requires significantly more proving.

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
