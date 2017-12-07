---
title: 'Video: Getting things done in Haskell'
description: 'How to Architect Medium to Large-Scale Haskell Applications'
tags: haskell
---

Someone alerted me that the video of my talk at the Skills Matter [Haskell
eXchange 2017] is now available.  You can watch it [on their website].

[Haskell eXchange 2017]: https://skillsmatter.com/conferences/8522-haskell-exchange-2017
[on their website]: https://skillsmatter.com/skillscasts/10832-how-to-architect-medium-to-large-scale-haskell-applications

The slides can be found
[here](https://github.com/jaspervdj/talks/blob/master/2017-haskell-exchange-getting-things-done/slides.md).

It's a talk aimed towards beginners.  If you are writing a medium-sized Haskell
application for the very first time, you will typically end up with three
modules: `Types.hs`, `Utils.hs` and `Main.hs`.  While this is a very clear
split, it typically doesn't scale very well as applications become larger.

I try to answer some questions like:

- When is it a good idea to use something like Monad/Applicative (and when is
  it not)?
- When is it a good idea to invent my own typeclass (and when is it not)?
- How do I design interfaces and services like in OOP?

Thanks again to Skills Matter for putting together this excellent conference.
