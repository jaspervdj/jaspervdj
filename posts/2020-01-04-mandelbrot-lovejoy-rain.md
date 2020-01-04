---
title: "Mandelbrot & Lovejoy's Rain fractals"
description: Elegant but relatively useless noise
tags: 'haskell'
---

# Summary

At some point during [ICFP2019] in Berlin, I came across a completely unrelated
[old paper][pdf] by S. Lovejoy and B. B. Mandelbrot called _"Fractal properties
of rain, and a fractal model"_.

While the model in the paper is primarily meant to model rain**fall**; the
authors explain that it can also be used for rain**clouds**, since these two
phenomena are naturally similarly-shaped.  This means it can be used to generate
pretty pictures!

![](/images/2020-01-04-2d.jpg)

While it looked cool at first, it turned out to be an extremely pointless and
outdated way to generate pictures like this.  But I wanted to write it up anyway
since it is important to document failure as well as success: if you've found
this blogpost searching for an implementation of this paper; well, you have
found it, but it probably won't help you.
[Here is the GitHub repository][repo].

# The good parts

I found this paper very intriguing because it promises a fractal model with a
number of very attractive features:

 -  is extremely simple
 -  has easy to understand parameters
 -  is truly self-similar at different scales
 -  it has great [lacunarity]
    (I must admit I didn't know this word before going through this paper)

Most excitingly, it's possible to do a dimension-generic implementation!  The
code has examples in 2D as well as 3D (xy, time), but can be used without
modifications for 4D (xyz, time) and beyond.  Haskell's type system allows
capturing the dimension in a type parameter so we don't need to sacrifice any
type safety in the process.

For example, here the dimension-generic distance function I used with [massiv]:

```haskell
distance :: M.Index ix => ix -> ix -> Distance
distance i j = Distance . sqrt .
    fromIntegral .  M.foldlIndex (+) 0 $
    M.liftIndex2 (\p s -> (p - s) * (p - s)) i j
```

Here is a 3D version:

<div style="text-align: center;">
<iframe width="100%" height="315" src="https://www.youtube.com/embed/KRZ_6Rh6prE" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

# The (really) bad parts

However, there must be a catch, right?  If it has all these amazing properties,
why is nobody using it?  I didn't see any existing implementations; and even
though I had a very strong suspicion as to why that was the case, I set out to
implement it during [Munihac 2019].

As I was working on it, the answer quickly became apparent -- the algorithm is
so slow that its speed cannot even be considered a trade-off, its slowness
really cancels out all advantages and then some!  BitCoin may even be a better
use of compute resources.  The 30 second video clip I embedded earlier took 8
hours to render on a 16-core machine.

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
faster use a CPU (or, you know, 16 CPUs) and just draw that specific region onto
a single image.

The paper proposes 3 shapes (which it calls "pulses").  It starts out with just
drawing plain opaque circles with a hard edge.  This causes some interesting but
generally bad-looking edges:

![Hard circular pulses](/images/2020-01-04-hard.jpg)

It then switches to using circles with smoothed edges; which looks much better,
we're getting properly puffy clouds here:

![Smooth circular pulses](/images/2020-01-04-smooth.jpg)

Finally, the paper discusses drawing smoothed-out [annuli][annulus], which
dramatically changes the shapes of the clouds:

![Annular pulses](/images/2020-01-04-annuli.jpg)

It's mildly interesting that the annuli become hollow spheres in 3D.

Thanks to [Alexey] for [massiv] and a massive list of [suggestions] on my
implementation!

[Alexey]: https://alexey.kuleshevi.ch/
[ICFP2019]: https://icfp19.sigplan.org/
[Munihac 2019]: https://munihac.de/2019.html
[annulus]: https://en.wikipedia.org/wiki/Annulus_(mathematics)
[lacunarity]: https://en.wikipedia.org/wiki/Lacunarity
[massiv]: https://github.com/lehins/massiv/blob/master/README.md
[pdf]: https://www.tandfonline.com/doi/pdf/10.3402/tellusa.v37i3.11668
[repo]: https://github.com/jaspervdj/mandelbrot-lovejoy-rain/.
[suggestions]: https://github.com/jaspervdj/mandelbrot-lovejoy-rain/pull/1
