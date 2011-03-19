---
title: How I write a Haskell webapp
description: A simple set of design rules to create simple Haskell webapps
tags: haskell, code
---

What is this?
-------------

I recently gave a talk at a [the functional programming exchange] and
afterwards, I was talking to a couple of interesting people, and soon we were
discussing the design (that is, from a software engineering point of view) of
Haskell web applications.

[the functional programming exchange]: 

Disclaimer
----------

I've called this blogpost "How I write a Haskell webapp" and not "How to write a
Haskell webapp" for a reason: this is the way *I* would write a simple webapp.
You might or might not agree with me, but even if you don't agree, I guess it's
interesting to hear how others do it.  I don't know how these methods would
scale for larger sites -- I've thought about it a little, but never actually
tried creating a large-scale site this way.

Libraries
---------

A very general approach is used, which should work with most libraries. I'm
using [snap], [blaze-html] and [jQuery], but if you want you can use
[happstack], [heist] and [prototype] -- the methods used should carry over quite
easily.

[snap]: http://snapframework.com
[blaze-html]: http://jaspervdj.be/blaze
[jQuery]:
[happstack]:
[heist]:
[prototype]: 

MVC
---

I'm using an approach which looks like a Model-View-Controller architecture.
The terminology is not exactly right, but one of my professors told me that they
use the term MVC for everything nowadays, because it sounds hip.

The Model
---------

We don't use a very strict definition of MVC, and this is especially true for
the Model component. Usually we have *some sort of datatype* together with *some
functions*. These functions are usually pure. Since this is pretty abstract,
let's look at a concrete example:

~~~~~{.haskell}
-- Some code?
~~~~~

The View
--------

In a web application, the View is responsible for rendering our data to HTML (or
XML, or JSON...). We have something like:

~~~~~{.haskell}
view :: Data1 -> Data2 -> ... -> Html
~~~~~

The Controller
--------------

I think of the Controller as something which connects the the View and Model
with what people refer to as *the real world*.
