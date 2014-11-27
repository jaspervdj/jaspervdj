---
title: Comonads for Image Processing
description: A simple real-world example showing an image-processing Comonad
tags: haskell
---

Introduction
============

A Comonad is a structure from [category theory] dual to [Monad].

[category theory]: http://www.haskell.org/haskellwiki/Category_theory
[Monad]: http://www.haskell.org/haskellwiki/Monad

<blockquote>
Comonads are well-suited for image processing <br />
– Pretty much everyone on the internet
</blockquote>

Whenever Comonads come up, people usually mention the canonical example of
[evaluating cellular automatons]. Because many image processing algorithms can
be modelled as a cellular automata, this is also a frequently mentioned example.

[evaluating cellular automatons]: http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html

However, when I was trying to explain Comonads to a friend recently, I couldn't
find any standalone example of how exactly this applies to image processing, so
I decided to illustrate this myself.

I will not attempt to explain Comonads, for that I refer to Gabriel Gonzalez'
[excellent blogpost]. This blogpost is written in literate Haskell so you should
be able to just load it up in GHCi and play around with it (you can find the raw
`.lhs` file
[here](https://github.com/jaspervdj/jaspervdj/raw/master/posts/2014-11-27-comonads-image-processing.lhs)).

[excellent blogpost]: http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html

> {-# LANGUAGE BangPatterns #-}
> import qualified Codec.Picture       as Juicy
> import           Control.Applicative ((<$>))
> import           Data.List           (sort)
> import           Data.Maybe          (fromMaybe, maybeToList)
> import qualified Data.Vector         as V
> import qualified Data.Vector.Generic as VG
> import           Data.Word           (Word8)

A simple image type
===================

We need a simple type for images. Let's use the great [JuicyPixels] library to
read and write images. Unfortunately, we cannot use the image type defined in
JuicyPixels, since JuicyPixels stores pixels in a [Storable-based Vector].

[JuicyPixels]: http://hackage.haskell.org/package/JuicyPixels
[Storable-based Vector]: http://hackage.haskell.org/package/vector-0.10.11.0/docs/Data-Vector-Storable.html

We want to be able to store any kind of pixel value, not just `Storable` values,
so we declare our own `BoxedImage`. We will simply store pixels in row-major
order in a boxed `Vector`.

> data BoxedImage a = BoxedImage
>     { biWidth  :: !Int
>     , biHeight :: !Int
>     , biData   :: !(V.Vector a)
>     }

Because our `BoxedImage` allows any kind of pixel value, we get a
straightforward `Functor` instance:

> instance Functor BoxedImage where
>     fmap f (BoxedImage w h d) = BoxedImage w h (fmap f d)

Now, we want to be able to convert from a JuicyPixels image to our own
`BoxedImage` and back again. In this blogpost, we will only deal with grayscale
images (`BoxedImage Word8`), since this makes the image processing algorithms
mentioned here a lot easier to understand.

> boxImage :: Juicy.Image Juicy.Pixel8 -> BoxedImage Word8
> boxImage image = BoxedImage
>     { biWidth  = Juicy.imageWidth image
>     , biHeight = Juicy.imageHeight image
>     , biData   = VG.convert (Juicy.imageData image)
>     }

> unboxImage :: BoxedImage Word8 -> Juicy.Image Juicy.Pixel8
> unboxImage boxedImage = Juicy.Image
>     { Juicy.imageWidth  = biWidth boxedImage
>     , Juicy.imageHeight = biHeight boxedImage
>     , Juicy.imageData   = VG.convert (biData boxedImage)
>     }

Focused images
==============

While we can already write simple image processing algorithms (e.g. tone
mapping) using just the `Functor` interface, the kind of algorithms we are
interested in today need take a *neighbourhood* of input pixels in order to
produce a single output pixel.

For this purpose, let us create an additional type that focuses on a specific
location in the image. We typically want to use a smart constructor for this, so
that we don't allow focusing on an `(x, y)`-pair outside of the `pBoxedImage`.

> data Pixel a = Pixel
>     { pBoxedImage :: !(BoxedImage a)
>     , pX          :: !Int
>     , pY          :: !Int
>     }

Conversion to and from a `BoxedImage` is easy:

> toPixel :: BoxedImage a -> Pixel a
> toPixel bi
>     | biWidth bi > 0 && biHeight bi > 0 = Pixel bi 0 0
>     | otherwise                         =
>         error "Cannot create pixel for empty images"

> fromPixel :: Pixel a -> BoxedImage a
> fromPixel (Pixel bi _ _) = bi

And the functor instance is straightforward, too:

> instance Functor Pixel where
>     fmap f (Pixel bi x y) = Pixel (fmap f bi) x y

Now, we can implement the fabled Comonad class:

> class Functor w => Comonad w where
>     extract :: w a -> a
>     extend  :: (w a -> b) -> w a -> w b

The implementation of `extract` is straightforward. `extend` is a little
trickier. If we look at it's concrete type:

```haskell
extend :: (Pixel a -> b) -> Pixel a -> Pixel b
```

We want to convert all pixels in the image, and the conversion function is
supplied as `f :: Pixel a -> b`. In order to apply this to all pixels in the
image, we must thus create a `Pixel` for every position in the image. Then, we
can simply pass this to `f` which gives us the result at that position.

> instance Comonad Pixel where
>     extract (Pixel bi x y) =
>         biData bi V.! (y * biWidth bi + x)
>
>     extend f (Pixel bi@(BoxedImage w h _) x y) = Pixel
>         (BoxedImage w h $ V.generate (w * h) $ \i ->
>             let (y', x') = i `divMod` w
>             in f (Pixel bi x' y'))
>         x y

Proving that this instance adheres to the Comonad laws is a bit tedious but not
that hard if you make some assumptions such as:

```haskell
V.generate (V.length v) (\i -> v V.! i) = v
```

We're almost done with our mini-framework. One thing that remains is that we
want to be able to look around in a pixel's neighbourhood easily. In order to do
this, we create this function which shifts the focus by a given pair of
coordinates:

> neighbour :: Int -> Int -> Pixel a -> Maybe (Pixel a)
> neighbour dx dy (Pixel bi x y)
>     | outOfBounds = Nothing
>     | otherwise   = Just (Pixel bi x' y')
>   where
>     x'          = x + dx
>     y'          = y + dy
>     outOfBounds =
>         x' < 0 || x' >= biWidth bi ||
>         y' < 0 || y' >= biHeight bi

Median filter
=============

If you have ever taken a photo when it is fairly dark, you will notice that
there is typically a lot of noise. We'll start from this photo which I took a
couple of weeks ago, and try to reduce the noise in the image using our
Comonad-based mini-framework.

![The original image](/images/2014-11-27-stairs-original.png)

A really easy noise reduction algorithm is one that looks at a local
neighbourhood of a pixel, and replaces that pixel by the median of all the pixels
in the neighbourhood. This can be easily implemented using `neighbour`:

> reduceNoise1 :: Pixel Word8 -> Word8
> reduceNoise1 pixel = median
>     [ extract p
>     | x <- [-2, -1 .. 2], y <- [-2, -1 .. 2]
>     , p <- maybeToList (neighbour x y pixel)
>     ]

Note how our Comonadic filter takes the form of `w a -> b`. With a little
intuition, we can see how this is the dual of a monadic filter, which would be
of type `a -> m b`.

We used an auxiliary function which simply gives us the median of a list:

> median :: Integral a => [a] -> a
> median xs
>     | odd len   = sort xs !! (len `div` 2)
>     | otherwise = case drop (len `div` 2 - 1) (sort xs) of
>         (x : y : _) -> x `div` 2 + y `div` 2
>         _           -> error "median: empty list"
>   where
>     !len = length xs

![After applying reduceNoise1](/images/2014-11-27-stairs-reduce-noise-01.png)

Running this algorithm on our original picture already gives an interesting
result, and the noise has definitely been reduced. However, you will notice that
it has this watercolour-like look, which is not what we want.

Blur filter
===========

A more complicated noise-reduction filter uses a blur effect first. We can
implement a blur by replacing each pixel by a weighted sum of it's neighbouring
pixels.

This function implements the simple blurring kernel:

![](/images/2014-11-27-gaussian-blur-kernel.png)

> blur :: Pixel Word8 -> Word8
> blur pixel = fromMaybe (extract pixel) $ do
>     let self = fromIntegral (extract pixel) :: Int
>     topLeft     <- extractNeighbour (-1) (-1)
>     top         <- extractNeighbour   0  (-1)
>     topRight    <- extractNeighbour   1  (-1)
>     right       <- extractNeighbour   1    0
>     bottomRight <- extractNeighbour   1    1
>     bottom      <- extractNeighbour   0    1
>     bottomLeft  <- extractNeighbour (-1)   1
>     left        <- extractNeighbour (-1)   0
>     return $ fromIntegral $ (`div` 16) $
>         self * 4 +
>         top * 2 + right * 2 + bottom * 2 + left * 2 +
>         topLeft + topRight + bottomRight + bottomLeft
>   where
>     extractNeighbour :: Int -> Int -> Maybe Int
>     extractNeighbour x y = fromIntegral . extract <$> neighbour x y pixel

The result is the following image:

![The image after using blur](/images/2014-11-27-stairs-blurred.png)

This image contains less noise, but as we expected, it is blurry. This is not
unfixable though: if we subtract the blurred picture from the original picture,
we get the edges:

![The edges in the image](/images/2014-11-27-stairs-edge.png)

If we apply a high-pass filter here, i.e., we drop all edges below a certain
treshold, such that we only retain the "most significant" edges, we get
something like:

![The edges after applying a treshold](/images/2014-11-27-stairs-treshold.png)

While there is still some noise, we can see that it's clearly been reduced. If
we now add this to the blurred image, we get our noise-reduced image number #2.
The noise is not reduced as much as in the first image, but we managed to keep
more texture in the image (and not make it look like a watercolour).

![After applying reduceNoise2](/images/2014-11-27-stairs-reduce-noise-02.png)

Our second noise reduction algorithm is thus:

> reduceNoise2 :: Pixel Word8 -> Word8
> reduceNoise2 pixel =
>     let !original = extract pixel
>         !blurred  = blur pixel
>         !edge     = fromIntegral original - fromIntegral blurred :: Int
>         !treshold = if edge < 7 && edge > (-7) then 0 else edge
>     in fromIntegral $ fromIntegral blurred + treshold

We can already see how the Comonad pattern lets us combine `extract` and `blur`,
and simple arithmetic to achieve powerful results.

A hybrid algorithm
==================

That we are able to compose these functions easily is even more apparent if we
try to build a hybrid filter, which uses a weighted sum of the original,
`reduceNoise1`, and `reduceNoise2`.

> reduceNoise3 :: Pixel Word8 -> Word8
> reduceNoise3 pixel =
>     let !original = extract      pixel
>         !reduced1 = reduceNoise1 pixel
>         !reduced2 = reduceNoise2 pixel
>     in (original `div` 4) + (reduced1 `div` 4) + (reduced2 `div` 2)

The noise here has been reduced significantly, while not making the image look
like a watercolour. Success!

![After applying reduceNoise3](/images/2014-11-27-stairs-reduce-noise-03.png)

Here is our main function which ties everything up:

> main :: IO ()
> main = do
>     Right (Juicy.ImageY8 image) <- Juicy.readImage filePath
>     let pixel = toPixel $ boxImage image
>     Juicy.writePng "images/2014-11-27-stairs-reduce-noise-01.png" $
>         unboxImage $ fromPixel $ extend reduceNoise1 pixel
>     Juicy.writePng "images/2014-11-27-stairs-reduce-noise-02.png" $
>         unboxImage $ fromPixel $ extend reduceNoise2 pixel
>     Juicy.writePng "images/2014-11-27-stairs-reduce-noise-03.png" $
>         unboxImage $ fromPixel $ extend reduceNoise3 pixel
>   where
>     filePath = "images/2014-11-27-stairs-original.png"

Conclusion
==========

I hope this example has given some intuition as to how Comonads can be used in
real-world scenarios. For me, what made the click was realising how `w a -> b`
for Comonad relates to `a -> m b` for Monad, and how these types of functions
naturally compose well.

Additionally, I hope this blogpost provided some insight the image processing
algorithms as well, which I also think is an interesting field.
