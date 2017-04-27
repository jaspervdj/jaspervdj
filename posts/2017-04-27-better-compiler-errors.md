---
title: 'An informal guide to better compiler errors'
description: 'A talk in which I discuss compiler errors'
tags: haskell
---

Earlier this month I gave a talk at the [HaskellX Bytes] meetup on how to
improve compiler errors.

[HaskellX Bytes]: https://skillsmatter.com/groups/10656-haskell-bytes

Haskell is very commonly used to implement DSLs. When you implement one of these
DSLs, the focus is usually performance or correctness. Something that's not
mentioned often is the "quality" of error messages.

In this talk I talked through some techniques to improve various kinds (parser,
renamer, modules, interpreter, typechecker) of error messages and discuss how
that impacts user experience.

The recording can be watched
[on their website](https://skillsmatter.com/skillscasts/9879-an-informal-guide-to-better-compiler-errors-jasper-van-der-jeugt).
Unfortunately I cannot embed it here, but at least you can watch it without creating an account.

The slides can be found
[here](https://github.com/jaspervdj/talks/blob/master/2017-skillsmatter-errors/slides.md).
