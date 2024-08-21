---
title: Turnstyle
description: An esoteric, graphical functional language
tags: haskell
---

I am delighted and horrified to announce a new graphical programming language
called [Turnstyle].  You can see an example below (click to run).

<style type="text/css">
    img.turnstyle, .interpreter svg {
        max-width: 90%;
        margin-bottom: .5em; /* Some space in between svg and terminal */
    }

    .interpreter .terminal {
        text-align: left;
        display: block;
        overflow-y: scroll;
        max-height: 5em;
        background-color: #2228;
        color: #fff;
        padding: .5em;
    }

    .interpreter .terminal pre {
        margin: 0px;
    }

    .interpreter .terminal textarea {
        padding: 0px;
        margin: 0px;
        opacity: 0;
        width: 0px;
        height: 0px;
        border: none;
    }

    .interpreter .terminal:has(textarea:focus) .cursor {
        animation: cursor 1s linear infinite;
    }

    .interpreter .terminal .cursor {
        display: inline-block;
        height: 1.2em;
        margin-bottom: -0.1em;
        width: 0.5em;
        background: #fff;
    }

    @keyframes cursor {
        0%  {  background: transparent; }
        50% {  background: inherit;     }
    }
</style>

<img class="turnstyle" src="/images/2024-08-21-turnstyle-loop.svg">

---

In the time leading up to [ZuriHac 2024] earlier this year, I had been thinking
about [Piet] a little.  We ended up working on something else during the
Hackathon, but this was still in the back of my mind.

Some parts of Piets design are utter genius (using areas for number literals,
using hue/lightness as cycles).  There are also things I don't like, such as the
limited amount of colors, the difficulty reusing code, and the lack of a
way to extend it with new primitive operations.  I suspect these are part of the
reason nobody has yet tried to write, say, an RDBMS or a web browser in Piet.

Given the amount of attention going to programming languages in the functional
programming community, I was quite surprised nobody had ever tried to do a
functional variant of it (as far as I could find).

I wanted to create something based on Lambda Calculus.  It forms a nice basis
for a minimal specification, and I knew that while code would still be somewhat
frustrating to write, there is the comforting thought of being able to reuse
almost everything once it's written.

![Cheatsheet for the specification](/images/2024-08-21-turnstyle-cheatsheet.svg)

After playing around with different designs this is what I landed on.  The
guiding principle was to search for a specification that was as simple as
possible, while still covering lambda calculus extended with primitives that,
you know, allow you to interact with computers.

One interesting aspect that I discovered (not invented) is that it's actually
somewhat more expressive than Lambda Calculus, since you can build Abstract
Syntax Graphs (rather than just Trees).  This is illustrated in the loop example
above, which recurses without the need for a fixed-point combinator.

For the full specification and more examples take a look at the [Turnstyle
website][Turnstyle] and feel free to play around with the sources on [GitHub].

Thanks to [Francesco Mazzoli](https://mazzo.li/) for useful feedback on the
specification and website.

[GitHub]: https://github.com/jaspervdj/turnstyle/
[Turnstyle]: https://jaspervdj.be/turnstyle/
[ZuriHac 2024]: https://zfoh.ch/zurihac2024
[Piet]: https://www.dangermouse.net/esoteric/piet.html

<script type="text/JavaScript" src="/files/2024-08-21-turnstyle.js"></script>
<script type="text/JavaScript">
    window.onload = () => {
        const examples = document.querySelectorAll("img.turnstyle");
        for (const example of examples) {
            const source = example.src.replace(".svg", ".png");
            example.onclick = async (event) => {
                event.preventDefault();
                const itp = new Interpreter(document, source);
                await itp.load();
                example.replaceWith(itp.element);
                itp.run();
            };
        }
    };
</script>
