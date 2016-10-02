---
title: Patat and Myanmar travels
description: 'Presentations And The ANSI Terminal'
tags: haskell
---

# Presentations in the terminal

At [work], I frequently need to give (internal) presentations and demos using
video conferencing.  I prefer to do these quick-and-dirty presentations in the
terminal for a few reasons:

- I don't spend time worrying about layout, terminal stuff always looks cool.
- I want to write markdown if possible.
- You can have a good _"Questions?"_ slide just by running `cowsay 'Questions?'`
- Seamless switching between editor/shell and presentation using [tmux].

The last point is important for video conferencing especially.  The software we
use allows you to share a single window from your desktop.  This is pretty neat
if you have a multi-monitor setup.  However, it does not play well with
switching between a PDF viewer and a terminal.

[work]: http://fugue.co/
[tmux]: http://tmux.github.io/

# Introducing patat

To this end, I wrote [patat] -- **P**resentations **A**nd **T**he **A**NSI
**T**erminal -- because I was not entirely happy with the available solutions.
You can get it from [Hackage]: `cabal install patat`.

[patat]: https://github.com/jaspervdj/patat
[Hackage]: https://hackage.haskell.org/package/patat

![patat screenshot](/images/2016-10-02-screenshot.png)

You run it simply by doing:

    patat presentation.md

The key features are:

- **Built on Pandoc**:

    The software I was using before contained some Markdown parsing bugs.  By
    using [Pandoc] under the hood, this should not happen.

    Additionally, we get all the input formats Pandoc supports ([Literate
    Haskell] is of particular importance to me) and some additional elements
    like tables and definition lists.

- **Smart slide splitting**:

    Most Markdown presentation tools seem to split slides at `---` (horizontal
    rulers).  This is a bit verbose since you usually start each slide with an
    `h1` as well.  `patat` will check if `---` is used and if it's not, it will
    split on `h1`s instead.

- **Live reload**:

    If you run `patat --watch presentation.md`, `patat` will poll the file for
    changes and reload automatically.  This is really handy when you are writing
    the presentation, I usually use it with split-pane in `tmux`.

[Pandoc]: http://pandoc.org/
[Literate Haskell]: https://wiki.haskell.org/Literate_programming

An example of a presentation is:

~~~~~{.markdown}
---
title: This is my presentation
author: Jane Doe
...

# This is a slide

Slide contents.  Yay.

# Important title

Things I like:

- Markdown
- Haskell
- Pandoc
- Traveling
~~~~~

# How patat came to be

I started writing a simple prototype of `patat` during downtime at [ICFP2016],
when I discovered that [MDP] was not able to parse my presentation correctly.

[ICFP2016]: http://conf.researchr.org/home/icfp-2016
[MDP]: https://github.com/visit1985/mdp

After ICFP, I flew to Myanmar, and I am currently traveling around the country
with my girlfriend.  It's a super interesting place to visit, with a [rich
history].  Now that [NLD] is the ruling party, I think it is a great time to
visit the country responsibly.

[rich history]: https://newint.org/features/2008/04/18/history/
[NLD]: https://en.wikipedia.org/wiki/National_League_for_Democracy

![Riding around visiting temples in Bagan](/images/2016-10-02-bagan.jpg)

However, it is a **huge** country -- the largest in south-east Asia -- so there
is some downtime traveling on domestic flights, buses and boats.  I thought it
was a good idea to improve the tool a bit further, since you don't need internet
to hack on this sort of thing.

Pull requests are welcome as always!  Note that I will be slow to respond: for
the next three days I will be trekking from Kalaw to Inle Lake, so I have no
connectivity (or electricity, for that matter).

![Sunset at U Bein bridge](/images/2016-10-02-u-bein.jpg)

_Sidenote_: _"Patat"_ is the Flemish word for "potato".  Dutch people also use
it to refer to French Fries but I don't really do that -- in Belgium we just
call fries _"Frieten"_.
