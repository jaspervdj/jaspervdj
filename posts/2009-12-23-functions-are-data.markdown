---
title: Functions are data (and very cool data, too)
about: A short rant about how cool it is to use functions as data in Haskell.
tags: haskell, design
---

This blog post is a short rant about how people who come from an Object-Oriented
programming language (like me) are often using Haskell classes in a bad way.

## The problem

As a simple problem, suppose that we have a number of arbitrary shapes, and we
want to find out if a certain point lies in any of those shapes. First, let us
define a `Point` as an `(x, y)` pair.

~~~~~{.haskell}
type Point = (Float, Float)
~~~~~

What we want to do in pseudo-code is basically:

~~~~~{.haskell}
let ls = -- Create a list of shapes
any (\a -> inShape a (4, 3)) ls
-- Get back if (4, 3) is situated in any of the given shapes.
~~~~~

## An Object-Oriented approach

When coming from an Object-Oriented background, you would associate a shape with
an interface (in Java terminology). Haskell has a construct that looks a lot
like interfaces, called classes. We'll define a class `Shape`.

~~~~~{.haskell}
class Shape a where
    inShape :: a -> Point -> Bool
~~~~~

Okay, nice. Now let's define a `Rectangle`, which will be an instance of `Shape`.

~~~~~{.haskell}
data Rectangle = Rectangle Point Point
instance Shape Rectangle where
    inShape (Rectangle (x0, y0) (x1, y1)) (x, y) = x0 <= x && x <= x1 &&
                                                   y0 <= y && y <= y1
~~~~~

As you can see, we created a `Rectangle` datatype, and it's constructor takes
the left-top point and the right-bottom point. Let's add a `Circle`.

~~~~~{.haskell}
data Circle = Circle Point Float
instance Shape Circle where
    inShape (Circle (x0, y0) r) (x, y) = dx * dx + dy * dy <= r * r
        where dx = x0 - x
              dy = y0 - y
~~~~~

A `Circle` takes a center point and a radius. Note that, until now on, we are
writing our Haskell program in more or less the same way as we would write it
in Java. The real problem arises when we want to declare our list of arbitrary
shapes. In `ghci`:

~~~~~
>*BadShape> let ls = [Rectangle (10, 10) (11, 16), Circle (5, 5) 3]

<interactive>:1:39:
    Couldn't match expected type `Rectangle'
               against inferred type `Circle'
~~~~~


Oh yes, we should've thought of that: all elements in a list must be of the same
type[^1]. We're doing it wrong[^2].

[^1]: Liam O'Connor pointed me to the fact that you _could_ use classes here by
     using the
     [Existensial Quantification GHC extension](http://hackage.haskell.org/trac/haskell-prime/wiki/ExistentialQuantification).

[^2]: Another solution could be to use different data constructors for one data
     type called `Shape`, but then there wouldn't be the Haskell class - Java
     interface connection.

## A more functional approach

We need to stop thinking of classes here. Because a `Shape` is a collection of
arbitrary points, we could consider a `Shape` as a function. This function
would, for any `Point`, return `True` when this `Point` lies in the `Shape`.

~~~~~{.haskell}
type Shape = Point -> Bool
~~~~~

If we want to keep our `inShape` function, it would simple apply the `Shape`
function now. It is thus quite redundant, but could be defined, for
compatibility reasons, as:

~~~~~{.haskell}
inShape :: Shape -> Point -> Bool
inShape = ($)
~~~~~

Now we'll write our rectangle function again.

~~~~~{.haskell}
rectangle :: Point -> Point -> Shape
rectangle (x0, y0) (x1, y1) (x, y) = x0 <= x && x <= x1 &&
                                     y0 <= y && y <= y1
~~~~~

Note that we will use partial function application here - the `(x, y)`
argument will not be given if we create a rectangle. And our circle function:

~~~~~{.haskell}
circle :: Point -> Float -> Shape
circle (x0, y0) r (x, y) = dx * dx + dy * dy <= r * r
        where dx = x0 - x
              dy = y0 - y
~~~~~

Now we can solve our problem with `ghci`.

~~~~~
*GoodShape> let ls = [rectangle (10, 10) (11, 16), circle (5, 5) 3]
*GoodShape> :t ls
ls :: [Point -> Bool]
*GoodShape> any (\a -> inShape a (4, 3)) ls
True
~~~~~

As we said, the `inShape` area is kind of deprecated now, we could write our
query as:

~~~~~
>*GoodShape> any ($(4, 3)) ls
True
~~~~~

Conclusion? When learning Haskell, it sometimes really helps to forget
everything you know from object-oriented programming languages.
