---
title: "Mandelbrot & Lovejoy's Rain fractals"
description: Elegant but relatively useless noise
tags: 'haskell'
---

At some point during [ICFP2019] in Berlin, I came across a completely unrelated
[old paper] by Mandelbrot, Lovejoy about the fractal properties of rain.

It looked cool but turned out to be extremely pointless.  I wanted to write it
up anyway since it's important to document failure as well as success: if you've
found this blogpost searching for an implementation of this paper; well, you
have found it, but it probably won't help you.

[ICFP2019]: https://icfp19.sigplan.org/
[old paper]: https://www.physics.mcgill.ca/~gang/eprints/eprintLovejoy/Tellus.1985.small.pdf

TODO: Picture

While the model in the paper is primarily meant to model rain**fall**; the
authors explain that it can also be used for rain**clouds**, since these two
phenomena are naturally similarly-shaped.

I initially found the paper very intriguing as it promises a nice fractal model
for rainclouds that:

 -  is extremely simple
 -  has easy to understand parameters
 -  is truly self-similar at different scales
 -  it has great [lacunarity]
    (I must admit I didn't know the word before going through this paper)

[lacunarity]: https://en.wikipedia.org/wiki/Lacunarity

Most excitingly, it's possible to do a dimension-generic implementation!  The
code has examples in 2D as well as 3D (xy, time), but can be used without
modifications for 4D (xyz, time) and beyond.  Haskell's type system allows
capturing the dimension in a type parameter so we don't need to sacrifice any
type safety in the process.

Here is a 3D version:

<video width="600" controls="true">
  <source src="/images/2020-01-02-moving-clouds.mp4" type="video/mp4">
</video>

However, there must be a catch, right?  If it has all these amazing properties,
why is nobody using it?  I didn't see any existing implementations; and even
though I had a very strong suspicion as to why that was the case, I set out to
implement it during [Munihac 2019].

[Munihac 2019]: https://munihac.de/2019.html

As I was working on it, the answer quickly became apparent -- the algorithm is
so slow that its speed cannot even be considered trade-off, its slowness really
cancels out all advantages and then some!

This was a bit of a bummer on two fronts: the second one being that I wanted to
use this as a vehicle to learn some GPU programming; and it turned out to be a
bad fit for GPU programming as well.

At a very high-level, the algorithm repeats the following steps many, many
times:

1.  At random, pick a position in (or near) the image.
2.  Pick a size for your circular shape in a way that the probability of the
    size being larger than _P_ is _P⁻¹_.
3.  Draw the circular shape onto the image.

This sounds great for GPU programming; we could generate a large number of
images and then just sum them together.  However, the probability distribution
from step 2 is problematic.  Small (i.e. smaller than 3x3 pixel) shapes are so
common that it seems faster use a CPU and just draw that specific region onto a
single image.
