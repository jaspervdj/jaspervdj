---
title: 'Text/UTF-8: Call for Benchmarks'
description: A search for libraries and applications extensively using text
tags: haskell
---

I'm very glad that I have been accepted again this year for the [Google Summer
of Code] program for [haskell.org] (here is a [list] of accepted projects). My
project aims to improve the [text] library by converting it to internally use
UTF-8 instead of UTF-16.

[Google Summer of Code]: http://code.google.com/soc/
[haskell.org]: http://haskell.org/
[list]: http://www.reddit.com/r/haskell/comments/gxj1h/haskellorg_gsoc_accepted_projects_are_up/
[text]: http://hackage.haskell.org/package/text

UTF-8 and UTF-16 both have advantages and disadvantages, which actually makes it
a pretty complicated choice. I've written about this a little in my [proposal]
(especially see Tom Harper's master dissertation if you're interested in the
subject).

[proposal]: /files/2011-gsoc-text-utf8-proposal.html

To support a decision here on UTF-8 vs. UTF-16, lots of benchmarks will be
needed. Hence, this is the first focus of the GSoC project: collecting a large
benchmark suite which models real-world usage of the text library.

This is why I'd like to ask everyone who has written/knows libraries or
applications that use the text library extensively to inform me of these
efforts. The reverse dependencies list on Hackage is a good starting point for
me but it doesn't point out how popular these packages are and how intensively
they use the text library. I will then convert a subset of this code to a
benchmark suite using criterion.

Open source code means more reliable benchmarks, because I can publish the code
I used for them. However, I'm also willing to sign non-disclosure agreements if
this means I can try out what effects the changes have on large systems.

There's several ways to contact me: I've started a [thread] on Haskell-cafe you
can reply to, or you can mail me privately using `jaspervdj+text@gmail.com`.
Thanks in advance for any help!

[thread]: http://www.haskell.org/pipermail/haskell-cafe/2011-April/091424.html
