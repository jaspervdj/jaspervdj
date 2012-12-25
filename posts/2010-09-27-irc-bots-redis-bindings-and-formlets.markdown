---
title: IRC Bots, Redis bindings and Formlets
description: Stuff I've been working on the last month
tags: haskell
---

It's been a bit quiet on this blog -- my last post dates from the 9th of August.
But it's still September, which means I still haven't broken my goal of writing
at least one blogpost every month.

The new academic term has now started at Ghent University, and I thought it'd be
a good idea to write a bit on the side projects I've done after my
[Google Summer of Code project](http://jaspervdj.be/blaze) ended.

# Number Six

<img src="/images/2010-09-27-number-six.jpg" alt="Number Six"
     style="float: right; margin: 10px" />

[Number Six](http://github.com/jaspervdj/number-six) is a sexy IRC bot I've been
working on, originally meant for the [GhentFPG] IRC channel (`#ghentfpg` on
Freenode).

[GhentFPG]: http://www.haskell.org/haskellwiki/Ghent_Functional_Programming_Group

I've tried to abstract the gore IRC protocol away as much as possible. The
result is that writing handlers (plugins responding to IRC messages) can now
happen in an easy yet quite flexible way.

All handlers run in the `Irc` monad, which gives them access to many useful
variables such as the IRC command used in the incoming message, the sender, the
text... Number Six will also run your handlers in separate thread, so a crashing
plugin will have no effect on the rest of the bot.

This is a sample plugin that will reconnect the bot when it's kicked:

~~~~~{.haskell}
handler :: Handler String
handler = makeHandler "kick" $ return $ onCommand "KICK" $ do
    (channel : nick' : _) <- getParameters
    myNick <- getNick
    when (nick' == myNick) $ do
        sleep 3
        writeMessage "JOIN" [channel]
~~~~~

I've parameterized `Handler` on `String` -- it simply specifies the string type
used (`String` and `ByteString` are supported). I'm using an irrefutable pattern
match, because I know that a crashing plugin won't matter much.

Of course, the code contains a large number of utility functions to quickly
create simpler handlers.
[Here](http://github.com/jaspervdj/number-six/blob/master/NumberSix/Handlers/Binary.hs)
is the code of a handler that does some basic base conversions.

Storing data in a database is also very simple. There is a Redis util module
which will automagically generate keys that don't clash between handlers and
bots.
[Here](http://github.com/jaspervdj/number-six/blob/master/NumberSix/Handlers/Seen.hs)
is an example.

# Redis bindings

That leads us to the next subject. I've written a small wrapper around the
[Haskell Redis bindings] which provide a somewhat simpler interface. It only
supports a small subset of the bindings (coincidentally, this subset equals the
subset of functions I needed).

[Haskell Redis bindings]: http://hackage.haskell.org/package/redis

~~~~~{.haskell}
redis <- connect localhost defaultPort
itemSet redis "commander" ("Adama" :: String)
Just commander <- itemGet redis "commander"
putStrLn $ commander
~~~~~

My package can be found [here](http://hackage.haskell.org/package/redis-simple).
As you can see, I've made the bindings as simple as I could. However, if you
need full redis functionality, I still recommend the original package.

# Formlets

A week or so ago, I read
[this discussion](http://permalink.gmane.org/gmane.comp.lang.haskell.web/425)
on the state of Haskell web frameworks.

I noticed that [Chris Done](http://chrisdone.com/) was using both blaze-html and
xhtml. This is because the
[formlets](http://www.haskell.org/haskellwiki/Formlets) library can only output
xhtml, which makes it a bit cumbersome to make it work together with blaze-html.

So, I've ported formlets to blaze-html. The results should appear on Hackage
when the maintainers find time to look over my patches. In the meanwhile, you
can find my fork [here](http://github.com/jaspervdj/formlets).
