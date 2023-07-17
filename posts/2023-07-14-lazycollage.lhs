---
title: 'Functional Pearl: Lazy Collages'
description: circular programming something
...

Prelude
=======

This blogpost is written in [reproducible] [Literate Haskell], so we need some
imports, which can be safely skipped.

[reproducible]: TODO-link-to-nix-file
[Literate Haskell]: https://wiki.haskell.org/Literate_programming

<details><summary>Show me the imports anyway!</summary>

> {-# LANGUAGE DeriveFoldable    #-}
> {-# LANGUAGE DeriveFunctor     #-}
> {-# LANGUAGE DeriveTraversable #-}
> module Main where
> import qualified Codec.Picture          as JP
> import qualified Codec.Picture.Types    as JP
> import           Control.Monad          (foldM)
> import           Control.Monad.ST       (runST)
> import           Data.Bifunctor         (first)
> import           Data.Bool              (bool)
> import           Data.Foldable          (for_)
> import           Data.List              (isSuffixOf)
> import           Data.List.NonEmpty     (NonEmpty (..))
> import           Data.Maybe             (fromMaybe)
> import           Data.Traversable       (for)
> import           System.Directory       (listDirectory)
> import           System.Environment     (getArgs)
> import           System.FilePath        ((</>))
> import           System.Random          (RandomGen, newStdGen, randomR)
> import           System.Random.Stateful (randomM, runStateGen)
> import           Text.Read              (readMaybe)

</details>

Introduction
============

Haskell is not my only interest --- I have also been quite into photography for
the past decade.  Recently, I was considering moving some of the stuff I have
on various social networks to a self-hosting solution.

Tumblr in particular has a fairly nice way to do photo sets, where these can
be organized in rows and columns.  I wanted to see if I could mimic this a
nicely recursive way, where rows and columns can be subdivided further.

One important constraint is that is that we want to present each picture as
the photographer envisioned it: concretely, we can scale it up our down (if
we preserve the aspect ratio!), but we can't crop out parts.

Order is important in photo essays, so we want the author to specify the photo
collage in a declarative way by indicating if horizontal (H) or vertical (V)
subdivision should be used, creating a tree.  For example:

    H img1.jpg
      (V img2.jpg
         (H img3.jpg
            img4.jpg))

The program should then determine the exact size and position of each image,
so that we get a fully filled rectangle without any borders or filler:

![](../images/2023-07-14-lazycollage-example-1.jpg)

TODO: the next two paragraphs are kinda bad:

We will use a technique called _circular programming_ that builds on Haskell's
laziness to achieve this in an elegant way.
These days, it is maybe more commonly referred to as the `repmin` problem.
This was first described by Richard S. Bird in _"Using circular programs to
eliminate multiple traversals of data"_ in 1984, which **predates Haskell!**

<details><summary>What is <code>repmin</code>? I've never heard of it!</summary>

Interlude: repmin
-----------------

Given a simple tree type:

> data Tree a = Leaf a | Branch (Tree a) (Tree a)

We would like to write a function `repmin` which replaces each value in each
`Leaf` with the global minimum in the tree.  This is easily done by first
finding the global minimum, and then replacing it everywhere:

> repmin_2pass :: Ord a => Tree a -> Tree a
> repmin_2pass t = let globalmin = findmin t in rep globalmin t
>   where
>     findmin (Leaf x)     = x
>     findmin (Branch l r) = min (findmin l) (findmin r)
>
>     rep x (Leaf _)     = Leaf x
>     rep x (Branch l r) = Branch (rep x l) (rep x r)

However, this requires two passes over the tree.  We can do better by using
Haskell's laziness:

> repmin_1pass :: Ord a => Tree a -> Tree a
> repmin_1pass t = t'
>   where
>     (t', globalmin) = repmin t
>     repmin (Leaf   x)   = (Leaf globalmin, x)
>     repmin (Branch l r) =
>         let (l', lmin) = repmin l
>             (r', rmin) = repmin r in
>         (Branch l' r', min lmin rmin)

For more details, please see the original paper: despite (or because of?)
being almost 40 years old, it is surprisingly readable.

</details>

Lazy Collages
=============

We start out simple by giving an elegant algebraic definition for a collage:

> data Collage a
>     = Singleton  a
>     | Horizontal (Collage a) (Collage a)
>     | Vertical   (Collage a) (Collage a)
>     deriving (Foldable, Functor, Show, Traversable)

We will use the [JuicyPixels] library to load and write images.
The image type in this library can a bit verbose since it is parameterized
around the colour space.
During the layout pass, we don't really care about this, we
only need the relative sizes of the images and not their content.
We introduce a typeclass to do just that:

[JuicyPixels]: https://hackage.haskell.org/package/JuicyPixels

> data Size = Size
>     { sizeWidth  :: Rational
>     , sizeHeight :: Rational
>     } deriving (Show)
>
> class Sized a where
>     -- | Retrieve the width and height of an image.
>     -- Both numbers must be strictly positive.
>     sizeOf :: a -> Size

We use the `Rational` type for width and height.
We are only subdividing the 2D space, so we do not need irrational numbers,
and having infite precision is convenient.

The instance for the JuicyPixels image type is simple:

> instance Sized (JP.Image p) where
>     sizeOf img = Size
>         { sizeWidth  = fromIntegral $ JP.imageWidth  img
>         , sizeHeight = fromIntegral $ JP.imageHeight img
>         }

Let's think about the _output_ of our layout algorithm next.

![](../images/2023-07-14-lazycollage-tree-1.jpg)

If we look at the finished image, it may seem like a hard problem to find a
configuration that fits all the images with a correct aspect ratio.

But we can use induction to arrive at a fairly straightforward solution.  Given
two images, it is always possible to put them beside or above each other
by scaling them up or down to match them in height or width respectively.
Repeating this process, we can render the entire collage.

This is a bit of a naive approach since we end up making way too many copies.
Instead, we want to compute the layout and then render everything in one go.

There are some similarities with the algorithms present in browser engines.
TODO: One pass description.

The position of each individual input image can be represented
using _(x, y)_ coordinates.
We can also scale images up or down, and since we want to preserve the aspect
ratio, we can use a single _scale_ factor rather than having separate factors
for _x_ and _y_:

> data Transform = Tr
>     { trX     :: Rational
>     , trY     :: Rational
>     , trScale :: Rational
>     } deriving (Show)

Armed with the `Size` and `Transform` types, we have enough to tackle the
"mathy" bits.  Let's look at the horizontal case first.

We want to place image `l` beside image `r`, producing a nicely filled
rectangle.  Intuitively, we should be matching the height of both images.

There are different ways to do this -- we could enlarge the smaller
image, shrink the bigger image, or something in between.  We make a choice
to always shrink the bigger image, as this doesn't compromise the sharpness
of the result.

`horizontal` computes a transform for the left and right images, and a
total size:

> horizontal :: Size -> Size -> (Transform, Transform, Size)
> horizontal (Size lw lh) (Size rw rh) =
>   ( Tr 0         0 lscale
>   , Tr (ls * lw) 0 rscale
>   , Size width height
>   )
> where
>   height = min lh rh
>   lscale  = height / lh
>   rscale  = height / rh
>   width   = lscale * lw + rscale* rw

Composing images vertically is similar, matching the widths rather than the
heights of the two images:

> vertical :: Size -> Size -> (Transform, Transform, Size)
> vertical (Size tw th) (Size bw bh) =
>   ( Tr 0 0         ts
>   , Tr 0 (ts * th) bs
>   , Size width (ts * th + bs * bh)
>   )
> where
>   width = min tw bw
>   ts    = width / tw
>   bs    = width / bw in

Now that we've solved the problem of combining two images, we can apply this
to our tree of images.  We'll need to compose multiple transformations --
TODO: Monoid description

> instance Semigroup Transform where
>     Tr ax ay as <> Tr bx by bs =
>         Tr (ax + as * bx) (ay + as * by) (as * bs)

<details><summary>This seems suspicious, is this a valid Semigroup?</summary>

>     {-
>     Tr ax ay as <> (Tr bx by bs <> Tr cx cy cs)
>
>     -- Definition of <>
>     = Tr (ax + as * (bx + bs * cx))
>          (ay + as * (by + bs * cy))
>          (as * (bs * cs))
>
>     -- Distribute * over +
>     = Tr (ax + as * bx + as * bs * cx)
>          (ay + as * by + as * bs * cy)
>          (as * bs * cs)
>
>     -- Associativity of + and *
>     = Tr ((ax + as * bx) + (as * bs) * cx)
>          ((ay + as * by) + (as * bs) * cy)
>          ((as * bs) * cs)
>
>     -- Definition of <>
>     = (Tr ax ay as <> T bx by bs) <> Tr cx cy cs
>     -}

</details>

> instance Monoid Transform where
>     mempty = Tr 0 0 1

<details><summary>This seems suspicious, is this a valid Monoid?</summary>

>     {-
>     Tr ax ay as <> mempty
>
>     -- Definition of mempty
>     = Tr ax ay as <> Tr 0 0 1
>
>     -- Definition of <>
>     = Tr (ax + as * 0) (ay + as * 0) (as * 1)
>
>     -- Properties of 0 and 1 for *
>     = Tr ax ay as
>     -}

</details>

TODO: kill this:

> measure :: Sized img => Collage img -> Size
> measure (Singleton img) = sizeOf img
> measure (Horizontal l r) =
>     let (Size lw lh) = measure l
>         (Size rw rh) = measure r
>         height       = min lh rh
>         ls           = height / lh
>         rs           = height / rh in
>     Size (ls * lw + rs * rw) height
> measure (Vertical t b) =
>     let (Size tw th) = measure t
>         (Size bw bh) = measure b
>         width        = min tw bw
>         ts           = width / tw
>         bs           = width / bw in
>     Size width (ts * th + bs * bh)

TODO: It's probably better to define these before the monoid instance so
we see some concrete example of the Transform type before its used:

TODO: explain why it's always possible to create a collage like this.
explain we will do the maths first.  then write down these two functions:

We now have enough to write down the type signature of our main `collage`
function.  We will take the user-specified tree as input, and annotate
each element with a `Transform`.  In addition to that, we also produce the
`Size` of the final image so we can allocate space for it:

> collage :: Sized img => Collage img -> (Collage (img, Transform), Size)

All `collage` does is call `layout` --- our _circular_ program --- with
an initial _(x, y)_ position of _(0, 0)_ and the identity scale (1).

> collage = layout mempty

Now we can get down to business.

> layout
>     :: Sized img => Transform -> Collage img
>     -> (Collage (img, Transform), Size)

Placing a single image is easy, since we are passing in the scale and position
(at least in the base case: we will see soon that `transform` is actually
calculated in a circular way and depends on the output `Size`).

> layout transform (Singleton img) =
>     (Singleton (img, transform), sizeOf img)

The `Horizontal` and `Vertical` cases are very similar to each other.  We will
look at the `Horizontal` one in detail and then review `Vertical` as a summary.

> {-
> layout (Tr x y s) (Horizontal l r) =
> -}


Assuming `lw`, `lh` `rw`, and `rh` for the width and height of the left and
right images respectively, we can decide a `height` and scaling factor for both
images (`ls` / `rs`).  Putting this together, we can compute the width of the
putting the images beside each other, and `height` remains the same:

>     {-
>     let height = min lh rh    -- Need to match heights
>         ls     = height / lh  -- Scale for left image
>         rs     = height / rh  -- Scale for right image
>         size'  = Size (ls * lw + rs * rw) height
>     -}

NOT ESSENTIAL / SKIP: Due to our choice of picking the minimum height, it
follows that at least one of `ls` or `rs` will be 1.

We can now take the opposite view, completing the circular reasoning:
assuming `ls` and `rs` as scaling factor for the left and right images, and
passing in the transformation for both images by calling `layout` recursively:

>     {-
>         rx               = x + s * ls * lw  -- X of right image
>         (l', Size lw lh) = layout (Tr x  y (s * ls)) l
>         (r', Size rw rh) = layout (Tr rx y (s * rs)) r in
>     (Horizontal l' r', size')
>     -}

You may wonder how this can work: `ls` depends on `lh` which _seems_ to depend
on `ls` due to the recursive call!  But there is no actual circular dependency
since we can --- informally --- delay the evaluation of the `Transform` until
the very end.  We only use the `Transform` parameter passed in to `layout` to
compute the next `Transform`: so we can just keep building all of these
lazy `Transform` thunks.  If our tree is finite, the computation will not
diverge since we know at some point we will hit a `Singleton`.

The code for `Vertical` is the dual of the code we just went through for
`Horizontal` -- instead of right/left, we have top/bottom, and we use the
same width rather than the same height.

The apparent cycle is maybe more noticeable in this denser code: `layout` is
producing the size of both the top and bottom images, and we are using that
to calculate the transform which we pass in as the first argument to `layout`
again!

> layout transform (Horizontal l r) =
>     let (l', lsize) = layout (transform <> lt) l
>         (r', rsize) = layout (transform <> rt) r
>         (lt, rt, size) = horizontal lsize rsize in
>     (Horizontal l' r', size)

> layout transform (Vertical t b) =
>     let (t', tsize) = layout (transform <> tt) t
>         (b', bsize) = layout (transform <> bt) b
>         (tt, bt, size) = vertical tsize bsize in
>     (Vertical t' b', size)

Appendix A: rendering the result
================================

Once we've determined the layout, we still need to apply this and draw all
the images using the computed transformations.  We use simple nearest-neighbour
scaling since that is not the focus of this program, you could consider [lanzcos
interpolation] in a real application.

[lanzcos interpolation]: https://mazzo.li/posts/lanczos.html

> render
>     :: Foldable f
>     => Size
>     -> f (JP.Image JP.PixelRGB8, Transform)
>     -> JP.Image JP.PixelRGB8
> render (Size width height) images = runST $ do
>     canvas <- JP.createMutableImage (round width) (round height) black
>     for_ images $ transform canvas
>     JP.unsafeFreezeImage canvas
>   where
>     black = JP.PixelRGB8 0 0 0
>
>     transform canvas (img, Tr dstX dstY dstS) =
>         for_ [round dstX .. round (dstX + dstW) - 1] $ \outX ->
>         for_ [round dstY .. round (dstY + dstH) - 1] $ \outY ->
>            let inX = min (JP.imageWidth img - 1) $ round $
>                         fromIntegral (outX - round dstX) / dstS
>                inY = min (JP.imageHeight img - 1) $ round $
>                         fromIntegral (outY - round dstY) / dstS in
>            -- TODO: write out of bounds still seems possible!!!
>            JP.writePixel canvas outX outY $ JP.pixelAt img inX inY
>       where
>         dstW = fromIntegral (JP.imageWidth img)  * dstS
>         dstH = fromIntegral (JP.imageHeight img) * dstS

Appendix B: parsing a collage
=============================

We use a simple parser to allow the user to specify collages as a string, for
example on the command line.  This is a natural fit for [polish notation] as
using parentheses in command line arguments is very awkward.

As an example, we want to parse the following arguments:

    H img1.jpg V img2.jpg H img3.jpg img4.jpg

Into this tree:

    (Horizontal "img1.jpg"
        (Vertical "img2.jpg")
        (Horizontal "img3.jpg" "img4.jpg"))

[polish notation]: https://en.wikipedia.org/wiki/Polish_notation

We don't even need a parser library, we can just treat the arguments as a stack:

> parseCollage :: [String] -> Maybe (Collage FilePath)
> parseCollage args = do
>     (tree, []) <- parseTree args
>     pure tree
>   where
>     parseTree []         = Nothing
>     parseTree ("H" : stack0) = do
>         (x, stack1) <- parseTree stack0
>         (y, stack2) <- parseTree stack1
>         pure (Horizontal x y, stack2)
>     parseTree ("V" : stack0) = do
>         (x, stack1) <- parseTree stack0
>         (y, stack2) <- parseTree stack1
>         pure (Vertical x y, stack2)
>     parseTree (x   : stack0) = Just (Singleton x, stack0)

Appendix C: random collages
===========================

In order to test this program, I also added some functionality to generate
random collages.  This uses two helpers.

First, we have a function to pick a specified number of items from a list at
random:

> randomSample :: RandomGen g => Int -> [a] -> g -> Maybe ([a], g)
> randomSample n items gen0
>     | n <= 0     = Just ([], gen0)
>     | null items = Nothing
>     | otherwise  = case splitAt idx items of
>         (_,   [])          -> Nothing
>         (pre, item : post) ->
>             first (item :) <$> randomSample (n - 1) (pre ++ post) gen1
>     where
>     (idx, gen1) = randomR (0, length items - 1) gen0

Then, we have way to generate a collage tree from a non-empty list.  This works
by just inserting the items from the list one-by-one, deciding to insert them to
the left or right by picking random booleans.

> randomTree :: RandomGen g => NonEmpty a -> g -> (Collage a, g)
> randomTree (item0 :| items) gen0 = runStateGen gen0 $ \gen ->
>     foldM (insert gen) (Singleton item0) items
>   where
>     insert gen (Singleton x) item = do
>         constr <- bool Horizontal Vertical <$> randomM gen
>         pure $ constr (Singleton x) (Singleton item)
>     insert gen (Horizontal x y) item = do
>         left <- randomM gen
>         if left
>             then Horizontal <$> insert gen x item <*> pure y
>             else Horizontal <$> pure x <*> insert gen y item
>     insert gen (Vertical x y) item = do
>         left <- randomM gen
>         if left
>             then Vertical <$> insert gen x item <*> pure y
>             else Vertical <$> pure x <*> insert gen y item

Putting these two helpers together, we can write `randomCollage`:

> randomCollage :: RandomGen g => Int -> [a] -> g -> Maybe (Collage a, g)
> randomCollage num items gen0 = case randomSample num items gen0 of
>     Just (x : xs, gen1) -> Just $ randomTree (x :| xs) gen1
>     _                   -> Nothing

Appendix D: a quick CLI
=======================

We support two modes of operation for our little CLI:

 *   Using a user-specified collage using the parser we wrote before.
 *   Generating a random collage from a directory of images, possibly limiting
     the number of images.

In both cases, we also take an output file as the first argument, so we know
where we want to write the image to.

> data Command
>     = User   FilePath (Collage FilePath)
>     | Random FilePath FilePath (Maybe Int)
>     deriving (Show)

> parseCommand :: [String] -> Maybe Command
> parseCommand []              = Nothing
> parseCommand [out, dir]      = Just $ Random out dir Nothing
> parseCommand [out, dir, num] = Random out dir . Just <$> readMaybe num
> parseCommand (out : spec)    = User out <$> parseCollage spec

Time to put everything together in `main`.  First we'll do some parsing:

> main :: IO ()
> main = do
>     args <- getArgs
>     command <- maybe (fail "invalid command") pure $
>         parseCommand args
>     (output, pathsCollage) <- case command of
>         User output explicit -> pure (output, explicit)
>         Random output dir mbNum -> do
>             entries <- map (dir </>) <$> listDirectory dir
>             let num = fromMaybe (length entries) mbNum
>             gen <- newStdGen
>             case randomCollage num entries gen of
>                 Nothing          -> fail "no random collage found"
>                 Just (random, _) -> pure (output, random)

We've created a `Collage FilePath` at this point.  We can use the `Traversable`
instance of `Collage` to load all the images:

>     imageCollage <- for pathsCollage $ \path ->
>         JP.readImage path >>= either fail (pure . JP.convertRGB8)

This gives us a `Collage (JP.Image JP.PixelRGB8)`.  We can pass that to our
`layout` function and write it to the output:

>     let (result, box) = collage imageCollage
>     write output $ JP.ImageRGB8 $ render box result
>   where
>     write output
>         | ".jpg" `isSuffixOf` output = JP.saveJpgImage 80 output
>         | otherwise                  = JP.savePngImage output
