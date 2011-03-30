---
title: Hacking Hakyll
description: A bit on Hakyll internals for interested hackers
tags: haskell, hakyll
---

What is this
------------

I've recently [released] Hakyll 3, and it seems to have reached a certain form
of stability now. The documentation is getting better, especially after
[Benedict Eastough] ported the [examples] from Hakyll 2.x to 3.

However, recently it was brought to my attention that hacking on Hakyll is quite
difficult -- all source code is relatively clean and well-commented but there is
no *global* overview available anywhere. This is what I hope to fix with this
blogpost: I will attempt to give a whirlwind tour of the Hakyll internals, and a
high-level overview of how it all works together.

[released]: http://www.haskell.org/pipermail/web-devel/2011/000980.html
[Benedict Eastough]: http://extralogical.net/
[examples]: https://github.com/jaspervdj/hakyll/tree/master/examples

Core/Web
--------

The Hakyll module namespace is divided into two large groups: `Hakyll.Core` and
`Hakyll.Web`.

Hakyll is essentially a compilation system (from this perspective, it looks a
little like the [Makefiles] we all love and hate). On the other hand, it is
mostly aimed at creating static websites. So, the distinction is pretty simple:

- `Hakyll.Core`: everything related to the compilation system/dependency;
  management
- `Hakyll.Web`: everything related to web sites.

[Makefiles]: http://en.wikipedia.org/wiki/Make_(software)

An important constraint of this I imposed onto myself is that a module located
in `Hakyll.Core` can *absolutely never* depend on a module in `Hakyll.Web`.

Hakyll.Core.Compiler
--------------------

Apart from having the coolest name, this module is probably also the central
module in Hakyll (for future reference, when I say `Module`, I usually mean
`Module`
*and* `Module.*`).

It exposes the `Compiler a b` arrow, which is, from a high-level point of view
composed out of two things:

- A (Kleisli) arrow from `a` to `b`: this what will actually produce whatever
  you want. This could, for example, produce a website page from a markdown
  file.
- A function producing a dependency set, which lists all dependencies used in
  the arrow mentioned above.

Hakyll.Core.Run
---------------

This module can be called "the runtime system" of Hakyll. It is the module which
actually runs a `Compiler`. Running happens in two phases:

- We run the dependency functions for all compilers. From this, we can infer a
  dependency graph.
- Using this information, we now run the necessary compiler arrows (i.e. the
  items which are out-of-date).

Other modules in Hakyll.Core
----------------------------

Hakyll.Web
----------
