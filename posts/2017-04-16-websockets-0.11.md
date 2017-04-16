---
title: 'WebSockets 0.11'
description: 'Update on the Haskell WebSockets library'
tags: haskell
---

# Introduction

Today, I released version 0.11 of the Haskell [websockets] library.  Minor
changes include:

[websockets]: /websockets/

- Support for IPv6 in the built-in server, client and tests (thanks to [agentm]).
- Faster masking (thanks to [Dmitry Ivanov]).

But most importantly, this release adds support for the `permessage-deflate`
extension as described in [RFC 7692].  A big thanks go out to [Marcin Tolysz]
who first submitted patches for an implementation.  Unfortunately, merging these
patches turned out to be a [rocky road].

[agentm]: https://github.com/agentm
[Dmitry Ivanov]: https://github.com/ethercrow
[Marcin Tolysz]: https://github.com/tolysz

[RFC 7692]: https://tools.ietf.org/html/rfc7692
[rocky road]: https://github.com/jaspervdj/websockets/pull/130

# Autobahn

After merging all these changes and improving upon them, I'm very happy that the
library now passes the [Autobahn Testsuite].  This language-agnostic testsuite
is very extensive and contains test cases covering most of the protocol surface.

![Autobahn](/images/2017-04-16-autobahn.jpg)

[Autobahn Testsuite]: https://github.com/crossbario/autobahn-testsuite

When I started running this testsuite against the websockets library, it was
very encouraging to learn that -- apart from a few corner cases -- it was
already passing most of this testsuite without any additional work.

The majority of failing tests were caused by the problem that the Haskell
websockets library was _too lenient_: it would accept invalid UTF-8 characters
when reading the messages as a `ByteString`.  The RFC, however, dictates that a
server should immediately close the connection if the client sends invalid
UTF-8.

This has now been rectified, but as an _opt-in_, in order not to break any
existing applications using this library.  For example, in order to enable the
new compression and UTF-8 validation, you can use something like:

~~~~~{.haskell}
main :: IO ()
main = WS.runServerWith "0.0.0.0" 9001 options application
  where
    options = WS.defaultConnectionOptions
        { WS.connectionCompressionOptions =
            WS.PermessageDeflateCompression WS.defaultPermessageDeflate
        , WS.connectionStrictUnicode      = True
        }
~~~~~

Note that if you are already decoding the incoming messages to `Text` values
through the `WebSocketsData` interface, enabling `connectionStrictUnicode`
should not add any additional overhead.  On the other hand, if your application
is a proxy which just takes the `ByteString`s and sends them through, enabling
UTF-8 validation will of course come at a price.

In a future release, `permessage-deflate` will be enabled by default.

# Other things

I realise that this blog has been a little quiet lately.  This is because (aside
from work being busy) I've been involved in some other things:

- I have been co-organising [Summer of Haskell] 2017.  If you are a student, and
  you would like to make some money while contributing to the Haskell ecosystem
  this summer, please think about applying.  If you are a (Haskell) project
  maintainer and you would like to mentor a student, please consider adding an
  [idea] for a proposal.

- I am also co-organising [ZuriHac] 2017.  It looks like this event will be the
  largest Haskell Hackathon ever, with over 250 participants.  Unfortunately,
  the event is now full, but if you're interested you can still register -- we
  will add you to the waiting list.  We've seen about 10% cancellations each
  year, so there is still a good chance of getting into the event.

- Lastly, I have been playing [the new Zelda game], which has of course been a
  blast.

[Summer of Haskell]: http://summer.haskell.org/
[ZuriHac]: https://zurihac.info/
[idea]: https://summer.haskell.org/ideas.html
[the new Zelda game]: http://www.zelda.com/breath-of-the-wild/
