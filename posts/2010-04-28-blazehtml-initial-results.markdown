---
title: BlazeHtml: Initial results
description: BlazeHtml was accepted as GSoC project, I present the project here
tags: haskell, blazehtml
---

## What is this

This is a blogpost version of a talk I recently gave at the [DutchHUG], about
BlazeHtml, a "blazingly fast HTML combinator library". This project was
accepted for Google Summer of Code, so I thought I'd blog it, too.

[DutchHUG]: http://dutchhug.nl

## What is BlazeHtml

About one month ago, [a discussion] took place on the Haskell web-devel mailing
list. In the mail I link too, Chris Eidhof describes what he wants to see in the
Haskell web-devel neighbourhood. Not much later, a [ticket] is created for
Google Summer of Code by Johan Tibbel.

[a discussion]: http://www.haskell.org/pipermail/web-devel/2010/000096.html
[ticket]: http://hackage.haskell.org/trac/summer-of-code/ticket/1580

I was quite interested in this idea from the start, so because I was attending
[ZuriHac], I started a [project] on GitHub to do some work in this direction.

[ZuriHac]: http://www.haskell.org/haskellwiki/ZuriHac
[project]: http://github.com/jaspervdj/BlazeHtml

This project turned out to be more popular on ZuriHac than I had initially
expected -- I got the help from 7 awesome Haskellers: Chris Done, Fred Ross,
Jim Whitehead, Harald Holtmann, Oliver Mueller, Simon Meier and Tom Harper.

![Us hacking on ZuriHac]($root/images/2010-04-28-zurihac.jpg)

Together, we produced an initial, prototype version of BlazeHtml. With pain in
my heart, I have to say that I have had to throw all our code away and make a
fresh start after ZuriHac. But this does not mean the code we wrote was not
useful: I will reintegrate big parts of it later, and it was a very useful
learning experience.

## Current state

So, what is the current state of our library? After having tried different
design paths, we now have an implementation that is fast. With this baseline
performance, I hope to continue. The question, of course, is how fast it
exactly is.

The "Big Table" benchmark is a very simple microbenchmark, implemented in many
different templating engines. It times the rendering of a big `<table>`. This
table has 1000 rows and 10 columns, and every row has the simple content 1, 2,
3, ... 10.

The different templating engines tested are:

- [Spitfire](http://code.google.com/p/spitfire/)
- [ClearSilver](http://www.clearsilver.net/)
- [ERB](http://corelib.rubyonrails.org/classes/ERB.html)
- [Erubis](http://www.kuwata-lab.com/erubis/)
- Our own BlazeHtml.

![BigTable benchmarks]($root/images/2010-04-28-benchmarks.png)

For the record, all benchmarks are run on a Intel CPU T2080 @ 1.73GHz. As you
can see, BlazeHtml is quite fast. I also tried some other Haskell libraries,
most of them achieved results a little slower than spitfire.

## Why is BlazeHtml fast

The approach we initially had was composed of a number of typeclasses. While
typeclasses are good for abstraction and modularity reasons, they can also be a
serious bottleneck. A first step to get some more speed was to remove a number
of typeclasses.

Secondly, we were using the `Data.Binary.Builder` monoid to efficiently
construct lazy `ByteString`'s. The `Builder` turned out to be very fast, but the
problem was that we were introducing too much overhead. Every character was
taken through an entire pipeline of functions that looked more or less like

~~~~~{.haskell}
append . encode .  escape
~~~~~

What I wanted is to have these operations executed on some sort of stream rather
than on every character.

## Forking of Data.Binary.Builder

Originally, we were using the following function (defined in
`Data.Binary.Builder`) to create our `Builder` monoid:

~~~~~{.haskell}
singleton :: Word8 -> Builder
~~~~~

This function was called for every byte, introducing a severe overhead. I was
able to get significant speedups by defining the functions:

~~~~~{.haskell}
fromText :: Text -> Builder
fromUnescapedText :: Text -> Builder
fromShow :: Show a => a -> Builder
~~~~~

These functions were defined in `Text.Blaze.Internal.Utf8Builder`. They all rely
on a small patch to `Data.Binary.Builder`, exposing the following function:

~~~~~{.haskell}
-- | /O(n)./ A Builder from a raw write to a pointer.
--
fromUnsafeWrite :: Int                  -- ^ Number of bytes to be written.
                -> (Ptr Word8 -> IO ()) -- ^ Function that does the write.
                -> Builder              -- ^ Resulting 'Builder'.
~~~~~

As you can see from this signature, we have some code here that some people
would consider "not elegant". Those people include me. However, we must not
forget that efficiency is our main goal. Besides, these functions are not
exported to the end user.

## The Future

So, that's it for now. You can expect more updates from me in the feature,
since I'll be working on this project with great enthusiasm now that it has been
accepted to Google Summer of Code 2010. Thanks to all the people who made this
possible!
