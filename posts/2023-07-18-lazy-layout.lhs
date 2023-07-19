---
title: 'Lazy Layout'
description: A fun application of circular programming
tags: haskell
...

Prelude
=======

This blogpost is written in [reproducible] [Literate Haskell], so we need some
imports first.

[reproducible]: https://github.com/jaspervdj/jaspervdj/blob/master/posts/2023-07-18-lazy-layout.nix
[Literate Haskell]: https://wiki.haskell.org/Literate_programming

<details><summary>Show me the exact imports...</summary>

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
on various social networks to a self-hosted solution.

Tumblr in particular has a fairly nice way to do photo sets, where these can
be organized in rows and columns.  I wanted to see if I could mimic this in a
recursive way, where rows and columns can be subdivided further.

One important constraint is that is that we want to present each picture as
the photographer envisioned it: concretely, we can scale it up or down
(preserving the aspect ratio), but we can't crop out parts.

Order is also important in photo essays, so we want the author to specify the
photo collage in a declarative way by indicating if horizontal (H) or vertical
(V) subdivision should be used, creating a tree.  For example:

    H img1.jpg
      (V img2.jpg
         (H img3.jpg
            img4.jpg))

The program should then determine the exact size and position of each image,
so that we get a fully filled rectangle without any borders or filler:

![](../images/2023-07-18-lazy-layout-example-1.jpg)

We will use a technique called _circular programming_ that builds on Haskell's
laziness to achieve this in an elegant way.
These days, it is maybe more commonly referred to as the `repmin` problem.
This was first described by Richard S. Bird in _"Using circular programs to
eliminate multiple traversals of data"_ in 1984, which **predates Haskell!**

<details><summary>Give me a refresher on <code>repmin</code> please...</summary>

Interlude: repmin
-----------------

Given a simple tree type:

> data Tree a
>   = Leaf a
>   | Branch (Tree a) (Tree a)

We would like to write a function `repmin` which replaces each value in each
`Leaf` with the global minimum in the tree.  This is easily done by first
finding the global minimum, and then replacing it everywhere:

> repmin_2pass :: Ord a => Tree a -> Tree a
> repmin_2pass t =
>   let globalmin = findmin t in rep globalmin t
>  where
>   findmin (Leaf x)     = x
>   findmin (Branch l r) = min (findmin l) (findmin r)
>
>   rep x (Leaf _)     = Leaf x
>   rep x (Branch l r) = Branch (rep x l) (rep x r)

However, this requires two passes over the tree.  We can do better by using
Haskell's laziness:

> repmin_1pass :: Ord a => Tree a -> Tree a
> repmin_1pass t = t'
>  where
>   (t', globalmin) = repmin t
>   repmin (Leaf   x)   = (Leaf globalmin, x)
>   repmin (Branch l r) =
>     let (l', lmin) = repmin l
>         (r', rmin) = repmin r in
>     (Branch l' r', min lmin rmin)

For more details, please see the original paper
(`https://doi.org/10.1007/BF00264249`): despite being almost 40
years old, it is surprisingly readable.

</details>

Lazy Collages
=============

We start out simple by giving an elegant algebraic definition for a collage:

> data Collage a
>   = Singleton  a
>   | Horizontal (Collage a) (Collage a)
>   | Vertical   (Collage a) (Collage a)
>   deriving (Foldable, Functor, Show, Traversable)

We will use the [JuicyPixels] library to load and write images.
The image type in this library can be a bit verbose since it is parameterized
around the colour space.
During the layout pass, we don't really care about this, we
only need the relative sizes of the images and not their content.
We introduce a typeclass to do just that:

[JuicyPixels]: https://hackage.haskell.org/package/JuicyPixels

> data Size = Size
>   { sizeWidth  :: Rational
>   , sizeHeight :: Rational
>   } deriving (Show)
>
> class Sized a where
>   -- | Retrieve the width and height of an image.
>   -- Both numbers must be strictly positive.
>   sizeOf :: a -> Size

We use the `Rational` type for width and height.
We are only subdividing the 2D space, so we do not need irrational numbers,
and having infinite precision is convenient.

The instance for the JuicyPixels image type is simple:

> instance Sized (JP.Image p) where
>   sizeOf img = Size
>     { sizeWidth  = fromIntegral $ JP.imageWidth  img
>     , sizeHeight = fromIntegral $ JP.imageHeight img
>     }

If we look at the finished image, it may seem like a hard problem to find a
configuration that fits all the images with a correct aspect ratio.

But we can use induction to arrive at a fairly straightforward solution.  Given
two images, it is always possible to put them beside or above each other
by scaling them up or down to match them in height or width respectively.
Repeating this process, we can render the entire collage.

![](/images/2023-07-18-lazy-layout-tree-1.jpg)

However, this is a bit of a naive approach since we end up making way too many
copies.  We would like to compute the layout first, and then render everything
in one go.  Still, we can start by formalizing what happens for two images
and then work our way up.

We can represent the layout of an individual image by its position and size.
We use simple _(x, y)_ coordinates the position and a scaling factor
(relative to the original size of the image) for its size.

> data Transform = Tr
>   { trX     :: Rational
>   , trY     :: Rational
>   , trScale :: Rational
>   } deriving (Show)

Armed with the `Size` and `Transform` types, we have enough to tackle the
"mathy" bits.

Let's look at the horizontal case first and write a function that computes a
transform for both left and right images, as well as the size of the result.

> horizontal :: Size -> Size -> (Transform, Transform, Size)
> horizontal (Size lw lh) (Size rw rh) =

We want to place image `l` beside image `r`, producing a nicely filled
rectangle.  Intuitively, we should be matching the height of both images.

There are different ways to do this --- we could enlarge the smaller
image, shrink the bigger image, or something in between.  We make a choice
to always shrink the bigger image, as this doesn't compromise the sharpness
of the result.

>   let height = min lh rh
>       lscale = height / lh
>       rscale = height / rh
>       width  = lscale * lw + rscale * rw in

With the scale for both left and right images, we can compute the left
and right transforms.  The left image is simply placed at _(0, 0)_ and we need
to offset the right image depending on the (scaled) size of the left image.

>   ( Tr 0             0 lscale
>   , Tr (lscale * lw) 0 rscale
>   , Size width height
>   )

Composing images vertically is similar, just matching the widths rather than the
heights of the two images:

> vertical :: Size -> Size -> (Transform, Transform, Size)
> vertical (Size tw th) (Size bw bh) =
>   let width  = min tw bw
>       tscale = width / tw
>       bscale = width / bw
>       height = tscale * th + bscale * bh in
>   ( Tr 0 0             tscale
>   , Tr 0 (tscale * th) bscale
>   , Size width height
>   )

Now that we've solved the problem of combining two images and placing them,
we can apply this to our tree of images.  To this end, we need to compose
multiple transformations.

Whenever we think about composing things in Haskell, it's good to ask ourselves
if the thing we're trying to compose is a [Monoid].
In this case, `a <> b` means applying transformation `a` after transformation
`b`, so we will need to apply the scale of `b` to all parts of `a`:

[Monoid]: https://typeclasses.com/monoid

> instance Semigroup Transform where
>   Tr ax ay as <> Tr bx by bs =
>     Tr (ax * bs + bx) (ay * bs + by) (as * bs)

It's not immediately clear that this is a valid Semigroup, so we will be
rigorous and provide a proof that `a <> (b <> c) == (a <> b) <> c`.

<details><summary>Proof of associativity...</summary>

~~~~~{.haskell}
Tr ax ay as <> (Tr bx by bs <> Tr cx cy cs)

-- Definition of <>
= Tr (ax * (bs * cs) + (bx * cs + cx))
     (ay * (bs * cs) + (by * cs + cy))
     (as * (bs * cs))

-- Associativity of * and +
= Tr (ax * bs * cs + bx * cs + cx)
     (ay * bs * cs + by * cs + cy)
     ((as * bs) * cs)

-- Distributivity of * over +
= Tr ((ax * bs + bx) * cs + cx)
     ((ay * bs + by) * cs + cy)
     ((as * bs) * cs)

-- Definition of <>
= (Tr ax ay as <> T b by bs) <> Tr cx cy cs
~~~~~~

</details>

We also need to provide an identity `Transform`, which is just offsetting by 0
and scaling by 1:

> instance Monoid Transform where
>   mempty = Tr 0 0 1

Proving that the identity holds on `mempty` is simple so we will only do one
side, namely `a <> mempty == a`.

<details><summary>Proof of Monoid right identity...</summary>

~~~~~{.haskell}
Tr ax ay as <> mempty

-- Definition of mempy
= Tr ax ay as <> Tr 0 0 1

-- Definition of <>
= Tr (ax * 1 + 0) (ay * 1 + 0) (as * 1)

-- Cancellative property of 0 over +
-- Identity of 1 over *
= Tr ax ay as
~~~~~

</details>

Our main `collage` function takes the user-specified tree as input,
and annotates each element with a `Transform`.
In addition to that, we also produce the `Size` of the final image so we can
allocate space for it.

> collage
>   :: Sized img
>   => Collage img
>   -> (Collage (img, Transform), Size)

All `collage` does is call `layout` --- our _circular_ program --- with
the identity transformation:

> collage = layout mempty

`layout` takes the size and position of the current element as an argument,
and determines the sizes and positions of a tree recursively.

There are some similarities with the algorithms present in browser engines,
where a parent element will first lay out its children, and then use their
properties to determine its own width.

However, we will use Haskell's laziness to do this in a single top-down pass.
We provide a declarative algorithm and we leave the decision about what to
calculate when --- more concretely, propagating the size information about
the children back up the tree before constructing the transformations ---
to the compiler!

> layout
>   :: Sized img
>   => Transform
>   -> Collage img
>   -> (Collage (img, Transform), Size)

Placing a single image is easy, since we are passing in the scale and position,
and we return its requested size.  This is an important detail in making the
laziness work here: if we tried to return the _final_ size (including
the passed in transformation) rather than the _requested_ size, the computation
would diverge (i.e. recurse infinitely).

> layout trans (Singleton img) =
>   (Singleton (img, trans), sizeOf img)

In the recursive case for horizontal composition, we call the `horizontal`
helper we defined earlier with the left and right image sizes as arguments.
This gives us both transformations, that we can then pass in as arguments to
`layout` again – returning the left and right image sizes we pass in to the
`horizontal` helper, forming our apparent circle.

> layout trans (Horizontal l r) =
>   (Horizontal l' r', size)
>  where
>   (l', lsize)            = layout (ltrans <> trans) l
>   (r', rsize)            = layout (rtrans <> trans) r
>   (ltrans, rtrans, size) = horizontal lsize rsize

The same happens for the vertical case:

> layout trans (Vertical t b) =
>   (Vertical t' b', size)
>  where
>   (t', tsize)            = layout (ttrans <> trans) t
>   (b', bsize)            = layout (btrans <> trans) b
>   (ttrans, btrans, size) = vertical tsize bsize

It's worth thinking about why this works: the intuitive explanation is that
we can "delay" the execution of the transformations until the very end of the
computation, and then fill them in everywhere.  This works since no other parts
of the algorithm _depend_ on the transformation, only on the sizes.

Conclusion
==========

We've written a circular program!  Although I was aware of `repmin` for a long
time, it's not a technique I've applied often.  To me, it is quite interesting
because, compared to `repmin`:

 -  it is easier to explain to a novice why this is useful;
 -  it is perhaps easier to understand due to the visual aspect; and
 -  it is an example outside of the realm of parsers and compilers.

Thanks for reading!

What follows below are a number of relatively small functions that take care of
various tasks, included so this can function as a standalone program:

 -  [Actually rendering the layout back to an image](#rendering-the-result)
 -  [Parsing a collage description](#parsing-a-collage-description)
 -  [Generating random collages](#generating-random-collages)
 -  [Putting together the CLI](#putting-together-the-cli)

![](../images/2023-07-18-lazy-layout-example-2.jpg)

Appendices
==========

Rendering the result
--------------------

Once we've determined the layout, we still need to apply this and draw all
the images using the computed transformations.  We use simple nearest-neighbour
scaling since that is not the focus of this program, you could consider [lanzcos
interpolation] in a real application.

[lanzcos interpolation]: https://mazzo.li/posts/lanczos.html

> render
>   :: Foldable f
>   => Size
>   -> f (JP.Image JP.PixelRGB8, Transform)
>   -> JP.Image JP.PixelRGB8
> render (Size width height) images = runST $ do
>   canvas <- JP.createMutableImage (round width) (round height) black
>   for_ images $ transform canvas
>   JP.unsafeFreezeImage canvas
>  where
>   black = JP.PixelRGB8 0 0 0
>
>   transform canvas (img, Tr dstX dstY dstS) =
>     for_ [round dstX .. round (dstX + dstW) - 1] $ \outX ->
>     for_ [round dstY .. round (dstY + dstH) - 1] $ \outY ->
>       let inX = min (JP.imageWidth img - 1) $ round $
>                 fromIntegral (outX - round dstX) / dstS
>           inY = min (JP.imageHeight img - 1) $ round $
>                 fromIntegral (outY - round dstY) / dstS in
>       JP.writePixel canvas outX outY $ JP.pixelAt img inX inY
>    where
>     dstW = fromIntegral (JP.imageWidth img)  * dstS
>     dstH = fromIntegral (JP.imageHeight img) * dstS

Parsing a collage description
-----------------------------

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
>   (tree, []) <- parseTree args
>   pure tree
>  where
>   parseTree []             = Nothing
>   parseTree ("H" : stack0) = do
>     (x, stack1) <- parseTree stack0
>     (y, stack2) <- parseTree stack1
>     pure (Horizontal x y, stack2)
>   parseTree ("V" : stack0) = do
>     (x, stack1) <- parseTree stack0
>     (y, stack2) <- parseTree stack1
>     pure (Vertical x y, stack2)
>   parseTree (x   : stack0) = Just (Singleton x, stack0)

Generating random collages
--------------------------

In order to test this program, I also added some functionality to generate
random collages.  This uses two helpers.

First, we have a function to pick a specified number of items from a list at
random:

> randomSample :: RandomGen g => Int -> [a] -> g -> Maybe ([a], g)
> randomSample n items gen0
>   | n <= 0     = Just ([], gen0)
>   | null items = Nothing
>   | otherwise  = case splitAt idx items of
>     (_,   [])          -> Nothing
>     (pre, item : post) ->
>       first (item :) <$> randomSample (n - 1) (pre ++ post) gen1
>  where
>   (idx, gen1) = randomR (0, length items - 1) gen0

Then, we have way to generate a collage tree from a non-empty list.  This works
by just inserting the items from the list one-by-one, deciding to insert them to
the left or right by picking random booleans.

> randomTree :: RandomGen g => NonEmpty a -> g -> (Collage a, g)
> randomTree (item0 :| items) gen0 = runStateGen gen0 $ \gen ->
>   foldM (insert gen) (Singleton item0) items
>  where
>   insert gen (Singleton x) item = do
>     constr <- bool Horizontal Vertical <$> randomM gen
>     pure $ constr (Singleton x) (Singleton item)
>   insert gen (Horizontal x y) item = do
>     left <- randomM gen
>     if left
>       then Horizontal <$> insert gen x item <*> pure y
>       else Horizontal <$> pure x <*> insert gen y item
>   insert gen (Vertical x y) item = do
>     left <- randomM gen
>     if left
>       then Vertical <$> insert gen x item <*> pure y
>       else Vertical <$> pure x <*> insert gen y item

Putting these two helpers together, we can write `randomCollage`:

> randomCollage :: RandomGen g => Int -> [a] -> g -> Maybe (Collage a, g)
> randomCollage num items gen0 = case randomSample num items gen0 of
>   Just (x : xs, gen1) -> Just $ randomTree (x :| xs) gen1
>   _                   -> Nothing

Putting together the CLI
------------------------

We support two modes of operation for our little CLI:

 *   Using a user-specified collage using the parser we wrote before.
 *   Generating a random collage from a directory of images, possibly limiting
     the number of images.

In both cases, we also take an output file as the first argument, so we know
where we want to write the image to.

> data Command
>   = User   FilePath (Collage FilePath)
>   | Random FilePath FilePath (Maybe Int)
>   deriving (Show)

> parseCommand :: [String] -> Maybe Command
> parseCommand []              = Nothing
> parseCommand [out, dir]      = Just $ Random out dir Nothing
> parseCommand [out, dir, num] = Random out dir . Just <$> readMaybe num
> parseCommand (out : spec)    = User out <$> parseCollage spec

We will add one more auxiliary function to load all images in a collage.
Fortunately we can just use the `Traversable` instance for this.

> readCollage
>   :: Collage FilePath
>   -> IO (Collage (JP.Image JP.PixelRGB8))
> readCollage = traverse $ \path ->
>   JP.readImage path >>= either fail (pure . JP.convertRGB8)

Time to put everything together in `main`.  First we do some parsing:

> main :: IO ()
> main = do
>   args <- getArgs
>   command <- maybe (fail "invalid command") pure $
>     parseCommand args
>   (output, pathsCollage) <- case command of
>     User output explicit -> pure (output, explicit)
>     Random output dir mbNum -> do
>       entries <- map (dir </>) <$> listDirectory dir
>       let num = fromMaybe (length entries) mbNum
>       gen <- newStdGen
>       case randomCollage num entries gen of
>         Nothing          -> fail "no random collage found"
>         Just (random, _) -> pure (output, random)

Followed by actually loading in all the images:

>   imageCollage <- readCollage pathsCollage

This gives us the `Collage (JP.Image JP.PixelRGB8)`.  We can pass that to our
`layout` function and write it to the output:

>   let (result, box) = collage imageCollage
>   write output $ JP.ImageRGB8 $ render box result
>  where
>   write output
>     | ".jpg" `isSuffixOf` output = JP.saveJpgImage 80 output
>     | otherwise                  = JP.savePngImage output
