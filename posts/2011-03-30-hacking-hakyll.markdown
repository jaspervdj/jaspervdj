---
title: Hacking Hakyll
description: A bit on Hakyll internals for interested hackers
tags: haskell
---

What is this
============

I've recently [released] Hakyll 3, and it seems to have reached a certain form
of stability now. The documentation is getting better, especially after
[Benedict Eastaugh] ported the [examples] from Hakyll 2.x to 3.

However, recently it was brought to my attention that hacking on Hakyll is quite
difficult -- all source code is relatively clean and well-commented but there is
no *global* overview available anywhere. This is what I hope to fix with this
blogpost: I will attempt to give a whirlwind tour of the Hakyll internals, and a
high-level overview of how it all works together.

[released]: http://www.haskell.org/pipermail/web-devel/2011/000980.html
[Benedict Eastaugh]: http://extralogical.net/
[examples]: https://github.com/jaspervdj/hakyll/tree/master/examples

Core/Web
========

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

This is what `Hakyll.Core` looks like from a high-level point of view (the
arrows represent "using" relations):

![Hakyll.Core](/images/2011-03-30-core.png)

Hakyll.Core.Compiler
====================

Apart from having the coolest name, this module is probably also the central
module in Hakyll (for future reference, when I say `Module`, I usually mean
`Module` *and* `Module.*`).

It exposes the `Compiler a b` arrow, which is, from a high-level point of view
composed out of two things:

- A (Kleisli) arrow from `a` to `b`: this what will actually produce whatever
  you want. This could, for example, produce a website page from a markdown
  file.
- A function producing a dependency set, which lists all dependencies used in
  the arrow mentioned above.

Hakyll.Core.Run
===============

This module can be called "the runtime system" of Hakyll. It is the module which
actually runs a `Compiler`. Running happens in two phases:

- We run the dependency functions for all compilers. From this, we can infer a
  dependency graph.
- Using this information, we now run the necessary compiler arrows (i.e. the
  items which are out-of-date).

Other modules in Hakyll.Core
============================

Those are not the only modules in `Hakyll.Core`. A quick listing of some other
interesting modules:

- `Hakyll.Core.Identifier` exports an `Identifier` type which is used to
  globally identify different compilers;

- `Hakyll.Core.DirectedGraph` implements a data structure used for dependency
  analysis. However, in the future, this might move to
  `Hakyll.Core.DependencyAnalyzer` or even a separate package;

- `Hakyll.Core.ResourceProvider` provides an interface for basically reading
  files from the disk. But it is written as an abstract data type so one could
  add, for example, a backend which allows Hakyll to fetch data from a SQL
  database;

- `Hakyll.Core.Rules` exports a monad for the rules DSL. This DSL is quite
  simple, it's basically a `Writer` monad which yields a list of compilers;

- `Hakyll.Core.Routes` contains the types and functions used for routing. This
  is a very simple module for now, altough extensions [have been suggested];

[have been suggested]: http://groups.google.com/d/msg/hakyll/5gzxxwI4cu4/u7Um62wXCpYJ

- `Hakyll.Core.Configuration` exports the global `HakyllConfiguration`. This is
  a relatively small configuration data type, which I think is a good thing;

- `Hakyll.Core.Store` implements a simple key/value store which allows you to
  save types instantiating `Binary` in the `_cache` directory.

Hakyll.Web
==========

The `Hakyll.Web` modules are more loosely coupeled, they all provide some
specific feature which helps the user in creating static websites. For example,
the `Hakyll.Web.CompressCss` module provides CSS compression.

The `Hakyll.Web.Page` and `Hakyll.Web.Template` modules are more tightly
integrated and a little more tricky (more in the next section).

I think most hacking opportunities lay in `Hakyll.Web`: there's probably a whole
range of filter-like compilers I haven't thought of yet.

The life of page
================

I want to finish this blogpost by shedding some more light on the process of
rendering a page (it's probably the most commonly used feature of Hakyll).

Usually, a page is compiled using `pageCompiler`. This is nothing more but a
"sane default", with a pretty simple definition:

~~~~~{.haskell}
pageCompiler :: Compiler Resource (Page String)
pageCompiler =   readPageCompiler
             >>> addDefaultFields
             >>> arr applySelf
             >>> pageRenderPandoc
~~~~~

The first step is `readPageCompiler` -- this is an arrow defined as:

~~~~~{.haskell}
readPageCompiler :: Compiler Resource (Page String)
readPageCompiler =   getResourceString
                 >>> arr readPage
~~~~~

This makes sense -- `getResourceString` simply gets the resource contents (i.e.
the file contents) as a `String`, and `readPage` is a pure function which parses
a `String` into a `Page`. If you want to have a certain text transformation on
the entire file, you need to replace `readPageCompiler` by a custom arrow (which
will probably look like `getResourceString >>> custom >>> arr readPage`).

The second step is `addDefaultFields`. After parsing the `Page`, it knows all
metadata fields specified in the actual file. But there's other metadata we want
available as well: the URL of the page (`$$url$$`), the source path
(`$$path$$`), ... all these fields are added here.

We're going to fill up beatiful templates with these fields later, but we also
want to be able to use, say, `$$url$$` *in the page itself*. In order to
accomplish this, we use the `applySelf` function, which applies a page as a
template to itself.

After all this is done, we use `pageRenderPandoc` to render the page to HTML.
`pageRenderPandoc`, much like `pageCompiler` is a sane default, it could be
defined as:

~~~~~{.haskell}
pageRenderPandoc :: Compiler (Page String) (Page String)
pageRenderPandoc =   pageReadPandoc
                 >>> pageWritePandoc
~~~~~

The actual definition is a little different, but certainly not harder. Again,
the point is that it's a simple pipeline of some other arrows. If you want to
perform custom transformations on the pandoc document (this is pretty awesome,
since you can edit documents easily using a proper language and not just
regexes), it goes here: `pageReadPandoc >>> custom >>> pageWritePandoc`. For
more information on defining these kind of pipelines, you should have a look at
what `Hakyll.Web.Pandoc` provides.

I hope this gives *some sort* of idea on how to start hacking if you want to
extend Hakyll with a certain feature. But then again, do not hesitate to poke me
if you're not sure, I'd be glad to help you get started.
