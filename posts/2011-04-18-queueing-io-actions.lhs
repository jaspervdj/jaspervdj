---
title: Queueing IO actions
description: Creating a simple queue system for retryable IO actions
tags: haskell
---

Lately, I've been working a bit on an automated system to count laps for a
running contest called [12 Urenloop (Dutch)]. We're working on this application
with our [computer science club], and I'm writing the counting backend in
Haskell (the repo for the full application is [here]).

[12 Urenloop (Dutch)]: http://www.12urenloop.be/
[computer science club]: http://zeus.ugent.be/
[here]: http://github.com/ZeusWPI/12UrenLoop

I came accross a problem that turned out to have a quite easy and elegant
solution -- or, at least, more easy and elegant than I initially thought.

After cleaning it up a little, I decided to provide it here as a blogpost. I
think it's a decent example of how one can not only build useful abstractions
for pure problems in Haskell, but also for very imperative-like and impure code.
Additonally, I vaguely recall people on [#haskell] saying we need more
tutorials on real-world stuff.

[#haskell]: http://www.haskell.org/haskellwiki/IRC_channel

The problem is quite simple: a component generates a number of requests to a
REST API running on another machine. We need to implement this in a fail-safe
way: basically, the other machine or the network can go down for a while. In
this case we should cache all requests and try them again later (but in the same
order).

Since this is written in Literate Haskell, we first have a few imports you can
safely skip over.

> module Queue where
>
> import Control.Applicative ((<$>))
> import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, modifyMVar_)
> import Control.Concurrent (threadDelay, forkIO)
> import Control.Exception (try, IOException)
> import Control.Monad (forever)
> import Data.Sequence (Seq, (|>))
> import qualified Data.Sequence as S
> import System.IO (hPutStrLn, stderr)

Let's step away from the problem of making REST API calls and come up with a
more general, informal description: we have some sort of "action" which runs or
fails. If the action fails, we need it try it again later.

> data Retry = Retry | Done
>            deriving (Show, Eq, Ord)

The above datatype represents a nice return code for the "actions" we need. We
now define an "action" as a `Retryable`, simply any `IO` returning an exit code
as described above:

> type Retryable = IO Retry

We want our queue to be thread-safe. For this purpose, a simple [MVar] will do.
Our queue will be represented by a [Sequence]. We could also use a simpler queue
data structure as given by [Okasaki], but we'll stick with Sequence since it's
in the commonly used [containers] package.

[MVar]: http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html
[Sequence]: http://www.haskell.org/ghc/docs/7.0.2/html/libraries/containers-0.4.0.0/Data-Sequence.html
[Okasaki]: http://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504
[containers]: http://hackage.haskell.org/packages/archive/containers/latest/doc/html/

And so we define our `Queue`: an `MVar` for thread-safety around our actual
queue.

> newtype Queue = Queue {unQueue :: MVar (Seq Retryable)}

People unfamiliar with the Haskell language might be confused at this point: we
used `data`, `type`, and `newtype` -- three different ways to create a type!
Let's elaborate on this a little:

- `data` creates a full-blown data type. Using `data`, you can create
  constructors, records, all of it;

- `type` simply creates a type alias, it does not actually create a *new type*.
  Hence, `type` is mostly used for readability reasons: we can now write
  `Retryable` instead of `IO Retry` (while they still mean the same thing to the
  compiler!);

- `newtype` creates a new type, with the restriction that it is a *wrapper
  around another type*. This holds advantages from a type-safety point of view:
  we cannot accidentally mix up types, and the implementation of the `Queue` can
  be hidden from the user. `newtype` wrappers are optimized away at compile
  time, so introducing extra type-safety has no performance penalty!

A first operation we can define is the creation of new, empty `Queue`.

> makeQueue :: Int -> IO Queue

When an action fails, we will wait a specified delay before we try again. This
is the first parameter: the delay specified in seconds.

> makeQueue delay = do
>     queue <- Queue <$> newMVar S.empty
>     _ <- forkIO $ forever $ threadDelay (delay * 1000000) >> pop queue
>     return queue

The implementation is pretty straight-forward:

- we create a `Queue` around a new `MVar` which holds an empty sequence --
  representing an empty queue in our case;

- we fork a new thread which runs forever. This thread sleeps for the specified
  delay and then tries to pop an item of the queue (`pop` is defined later).
  then the pattern repeats;

This means `pop` will be called automatically -- this makes sense, since we want
failed actions to be retried whenever possible. The user never has to call `pop`
manually.

Pushing a new `Retryable` on the queue has an even simpler definition:

> push :: Queue -> Retryable -> IO ()
> push queue retryable = do
>     modifyMVar_ (unQueue queue) $ return . (|> retryable)
>     pop queue

We modify our thread-safe variable by appending (`|>` is appending to the right
side of a Sequence) our new `Retryable`. Then we immediately try to `pop` from
the queue. This final pop gives us the nice property that all actions are
performed immediately after being pushed in an *everything-works-fine-scenario*.

The definition of `pop` holds the main logic for our queue-based system, and
it's defition looks more complicated, mostly because we need to handle all
different cases:

> pop :: Queue -> IO ()
> pop queue@(Queue mvar) = do
>     q <- takeMVar mvar
>     if S.null q then putMVar mvar S.empty
>                 else do r <- q `S.index` 0
>                         case r of Retry -> putMVar mvar q
>                                   Done  -> do putMVar mvar (S.drop 1 q)
>                                               pop queue

The reasoning behind it, however, follows simple rules: if the queue we find is
empty, we restore an empty queue. Otherwise, we run the action we find in the
front of the queue (`S.index q 0`), and:

- if it fails, we restore the original queue;

- it this action finishes successfully, we drop it from the queue and attempt
 to pop another action.

These twenty-or-so lines of source code were all that is needed to implement our
thread-safe queueing system for IO actions! Now, we're going to add a little
more code to make it easier for users.

We first create a function to convert an `IO` action without any exit code into
a `Retryable`, simply assuming it succeeds:

> assumeSuccesful :: IO () -> Retryable
> assumeSuccesful action = action >> return Done

We can also create a wrapper takes a `Retryable` (usually one created by the
function above) and converts it to another `Retryable` which will yield `Retry`
when an `IOException` occurs:

> wrapIOException :: Retryable -> Retryable
> wrapIOException retryable = do
>     result <- try retryable
>     case result of Left e -> failed e >> return Retry
>                    Right r -> return r
>   where
>     failed :: IOException -> IO ()
>     failed e = hPutStrLn stderr $ "Queue.wrapIOException: " ++ show e

Other wrappers are possible -- for example, in the application I am writing, I
had an `IO` action which performed an HTTP request and returned `Done` only when
the HTTP response code is a 2xx success code.

If you want to play around with this code, let's define a simple test function:

> test :: IO ()
> test = do
>     queue <- makeQueue 1
>     let action = readFile "/tmp/foo" >>= putStr
>     push queue $ wrapIOException $ assumeSuccesful action

Load [this file] in GHCI, run `test`, wait for a bit, and then write something
to `/tmp/foo`. Success!

[this file]: http://github.com/jaspervdj/jaspervdj/blob/master/posts/2011-04-18-queueing-io-actions.lhs
