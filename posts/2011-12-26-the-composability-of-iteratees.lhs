---
title: The composability of Iteratees
tags: haskell
---

A bit of context
----------------

I've been working on a [websockets library] lately, and this blogpost is mostly
based upon work on that library. The code given in this blogpost is *not* the
code from the library, but a stand-alone simplification, to make it more
accessible to readers.

[websockets library]: http://jaspervdj.be/websockets

This blogpost is based upon the [enumerator] library by John Millikin. Multiple
implementations of the Iteratee concept exist, I chose this one because it's
currently the most popular implementation for the packages I work on (mostly
web-related). Additionally, alternatives to iteratees are [being developed] and
I am certainly very curious how this will turn out.

[enumerator]: http://hackage.haskell.org/package/enumerator
[being developed]: https://github.com/snoyberg/conduit

Iteratees are often lauded because they offer great performance characteristics
in comparison to lazy I/O. Another key feature provided by iteratees is
composability (even for pure operations!) -- to which I will focus this
blogpost. An understanding of Iteratees is not required for this blogpost, and
be aware that my goal is to merely spark your interest, not to fully explain
them.

> import Blaze.ByteString.Builder (Builder)
> import Data.ByteString (ByteString)
> import Data.Enumerator ((=$))
> import Data.Monoid (mappend, mempty)
> import qualified Blaze.ByteString.Builder as BB
> import qualified Data.Attoparsec as A
> import qualified Data.Attoparsec.Enumerator as A
> import qualified Data.ByteString as B
> import qualified Data.ByteString.Lazy as BL
> import qualified Data.Enumerator as E
> import qualified Data.Enumerator.List as EL

The problem
-----------

Different versions of the WebSockets protocol exist. We obviously want to share
as much code as possible in between these versions, to avoid duplication. We can
define common `Message` datatype which can be used for all versions:

> data Message
>     = TextMessage BL.ByteString
>     | PingMessage
>     | CloseMessage
>     deriving (Show)

Now, for each version, we want to be able to parse these messages. That means
that:

1. we agree on a common interface (e.g. `ByteString -> Message`);
2. we implement this interface for each version;
3. we choose the right implementation at runtime.

The first WebSockets drafts (refered to Hybi00) offered a very simple message
format. A parser could be implemented easily:

> parseMessageHybi00 :: A.Parser Message
> parseMessageHybi00 = error "Omitted for brevity"

However -- this is not the case for later versions of the protocol (refered to
as Hybi10). These later versions introduced *frames*. This allows an web
application to multiplex different messages onto the socket -- more
specifically, it can insert *control messages* (e.g. ping) in between messages
with huge payloads. This increases the complexity of our server, since we have
to demultiplex these frames again.

We have three types of frames in the simplification of the protocol:

> data FrameType = TextFrame | PingFrame | CloseFrame
>     deriving (Show)

And a frame has a type, a flag indicating whether or not this is the last frame,
and a payload.

> data Frame = Frame FrameType Bool ByteString
>     deriving (Show)

These are not hard to parse either:

> parseFrameHybi10 :: A.Parser Frame
> parseFrameHybi10 = error "Omitted for brevity"

But if we want to have a common interface, we will need to access the result as
a `Message`. Remember that, we want the Hybi00 and the Hybi10 implementations to
offer *the same* interface: simply put, something like `ByteString -> Message`.

In a more simple world, we would've been able to write a simple
`[Frame] -> Message` function. This is clearly not the case here, as many frames
can produce many messages. `[Frame] -> [Message]` looks better, but what do we
do with leftover input which belongs to some next message? We clearly need a
stateful demultiplexer.

A stateful demultiplexer
------------------------

Most demultiplexers are obviously stateful -- the reason why I mentioned this
here is because our demultiplexer will be *explicitly* stateful. Explicit state
is not always a good thing: when this state impacts your entire application, it
quickly becomes tedious to pass around and manage. But here, we can clearly
restrict the statefulness to the demultiplexer.

In our simplification, `TextMessage` is the only kind of message that can be
split up in frames. This simplifies our demultiplexer greatly: we only need to
keep track of the leftover input. This is *not* the case for the actual
implementation, so it's probably a good idea to see the demultiplexer as some
kind of black box, accessible only through the functions `emptyDemultiplexState`
and `demultiplex`.

> type DemultiplexState = Builder

> emptyDemultiplexState :: DemultiplexState
> emptyDemultiplexState = mempty

> demultiplex :: Frame -> DemultiplexState -> ([Message], DemultiplexState)
> demultiplex (Frame CloseFrame _ _)   ds = ([CloseMessage], ds)
> demultiplex (Frame PingFrame _ _)    ds = ([PingMessage], ds)
> demultiplex (Frame TextFrame fin bs) ds = case fin of
>     False -> ([], ds')
>     True  -> ([TextMessage $ BB.toLazyByteString ds'], emptyDemultiplexState)
>   where
>     ds' = ds `mappend` BB.fromByteString bs

Such a straightforward Haskell implementation is easy to read, debug and
understand, but having to manually keep track of the state is surely a
disadvantage... or not?

A common interface
------------------

At a first sight, a disadvantage of such a stateful demultiplexer is that we
clearly cannot use a simple interface such as:

> type Interface0 = ByteString -> Message

nor can we use (superficially) more complex interfaces such as:

> type Interface1 = [ByteString] -> [Message]

precisely because we have to keep track of the `DemultiplexState` for the Hybi10
version. Well, we can easily solve this:

> type Interface2 =  DemultiplexState
>                 -> ByteString -> ([Message], DemultiplexState)

...given this interface, it is possible to write a correct implementation for
Hybi00 as well as Hybi10: the Hybi00 implementation would simply ignore the
`DemultiplexState`, and the Hybi10 implementation would use it as input for the
`demultiplex` function.

This is clearly an example of when explicit state becomes tedious to manage:
suppose we add another stateful component for some Hybi9000 implementation.
Then, we would have to adapt our `Interface2` too also incorporate this state:
it seems we're going down a very dangerous road here.

Enter Iteratee
--------------

As the title and introduction of this blogpost suggested, there is a better way
to solve this problem using Iteratees. I'm not going to go through all Iteratee
concepts here, but you should be able to follow with a basic Haskell knowledge.

An `Enumeratee x y m b` is a stream transformer which turns values of the type
`x` to values of the type `y`. We'll agree on such an interface (feel free to
safely ignore the `m` and `b` parameters if you're unfamiliar with Iteratees):

> type Interface3 m b = E.Enumeratee B.ByteString Message m b

The implementation for Hybi00 looks fairly easy:

> implHybi00 :: Monad m => Interface3 m b
> implHybi00 = E.sequence $ A.iterParser parseMessageHybi00

The trick is that we turn our parser into an `Iteratee` using the `iterParser`
function from the [attoparsec-enumerator] library. Such an `Iteratee` reads
`ByteString`s as input, and produces our `Message`: by repeatedly applying this
parser (using `sequence`), we obtain the stream transformer we wanted.

[attoparsec-enumerator]: http://hackage.haskell.org/package/attoparsec-enumerator

This is not very impressive, since a simple function could've be enough for the
Hybi00 implementation. But let's look at the Hybi10 implementation:

> implFramesHybi00 :: Monad m => E.Enumeratee B.ByteString Frame m b
> implFramesHybi00 = E.sequence $ A.iterParser parseFrameHybi10

constructing a stream transformer from `ByteString` to `Frame` is similar to
what we did for Hybi00. But we can implement an additional stream transformer:

> framesToMessages :: Monad m => E.Enumeratee Frame Message m b
> framesToMessages = EL.concatMapAccum step emptyDemultiplexState
>   where
>     step ds frame = let (msgs, ds') = demultiplex frame ds in (ds', msgs)

And compose these stream transformers to obtain our implementation:

> implHybi10 :: Monad m => Interface3 m b
> implHybi10 = (implFramesHybi00 =$) . framesToMessages

Mission completed!

Conclusion
----------

I've demonstrated that Iteratees allow you to write clear, concise and
explicitly stateful Haskell code, which is still composable without having to
manually manage this state in higher levels of your application. This is not a
new thing: it is probably the essence of stream processing in Haskell. Yet
Iteratees combine this with great performance and elegance, making it a very
interesting feature.
