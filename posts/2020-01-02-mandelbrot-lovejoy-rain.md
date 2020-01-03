---
title: "Mandelbrot & Lovejoy's Rain fractals"
description: Elegant but relatively useless noise
tags: 'haskell'
---

# Summary

At some point during [ICFP2019] in Berlin, I came across a completely unrelated
[old paper] by Mandelbrot, Lovejoy about the fractal properties of rain.

While the model in the paper is primarily meant to model rain**fall**; the
authors explain that it can also be used for rain**clouds**, since these two
phenomena are naturally similarly-shaped.  This means it can be used to generate
pretty pictures!

While it looked cool at first, it turned out to be an extremely pointless and
outdated way to generate pictures like this.  But I wanted to write it up anyway
since it's important to document failure as well as success: if you've found
this blogpost searching for an implementation of this paper; well, you have
found it, but it probably won't help you.

[ICFP2019]: https://icfp19.sigplan.org/
[old paper]: https://www.physics.mcgill.ca/~gang/eprints/eprintLovejoy/Tellus.1985.small.pdf

# The good parts

TODO: Picture

I found this paper very intriguing because it promises a fractal model with a
number of very attractive features:

 -  is extremely simple
 -  has easy to understand parameters
 -  is truly self-similar at different scales
 -  it has great [lacunarity]
    (I must admit I didn't know this word before going through this paper)

[lacunarity]: https://en.wikipedia.org/wiki/Lacunarity

Most excitingly, it's possible to do a dimension-generic implementation!  The
code has examples in 2D as well as 3D (xy, time), but can be used without
modifications for 4D (xyz, time) and beyond.  Haskell's type system allows
capturing the dimension in a type parameter so we don't need to sacrifice any
type safety in the process.

Here is a 3D version:

<div style="text-align: center;">
<iframe width="100%" height="315" src="https://www.youtube.com/embed/KRZ_6Rh6prE" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

# The (really) bad parts

However, there must be a catch, right?  If it has all these amazing properties,
why is nobody using it?  I didn't see any existing implementations; and even
though I had a very strong suspicion as to why that was the case, I set out to
implement it during [Munihac 2019].

[Munihac 2019]: https://munihac.de/2019.html

As I was working on it, the answer quickly became apparent -- the algorithm is
so slow that its speed cannot even be considered trade-off, its slowness really
cancels out all advantages and then some!  BitCoin may even be a better use of
compute resources.  The 30 second video clip I embedded earlier took 8 hours to
render on a 16-core machine.

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
from step 2 is problematic.  Small (≤3x3) shapes are so common that it seems
faster use a CPU and just draw that specific region onto a single image.
