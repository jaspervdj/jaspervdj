---
title: Type-safe events
description: Type-safe event-based programming in Haskell
tags: haskell, code
---

This is some code I wrote a while ago. It is (mostly) based upon [Data Types a
la Carte], a great pearl by [Wouter Swierstra]. It uses some ideas discussed in
this paper to create a type-safe, extensible event-based framework in Haskell.

[Data Types a la Carte]: http://www.cs.ru.nl/~wouters/Talks/DutchHug2011.pdf
[Wouter Swierstra]: http://www.cs.ru.nl/~wouters/

This blogpost is written in Literate Haskell, meaning you should be able to
download and run it. It also means we're going to have some (relatively common)
language extentions and imports:

> {-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
>         MultiParamTypeClasses, OverlappingInstances, TypeOperators #-}

> import Control.Applicative (Applicative)
> import Control.Monad.Reader (ReaderT, ask, runReaderT)
> import Control.Monad.Trans (MonadIO, liftIO)

An extensible sum type
======================

The first job is to write an extensible sum type, which will be how we represent
events. Think of it as an extended

> data SumType = A | B | C

where we can add more constructors in different files, so it's somewhat more
flexible. The `Contains` a typeclass means that a value of type `s` optionally
contains a value of type `a`. We can `wrap` and `unwrap` this type:

> class Contains a s where
>     wrap   :: a -> s
>     unwrap :: s -> Maybe a

Our main instance is a sum type combining two other types:

> data a :+: b = L a | R b
>              deriving (Show)
> infixr 5 :+:

Later, we will chain this sum type to a list like:

> type SomeNumber = Int :+: Float :+: Double :+: Integer

We need instances of `Contains` so we can wrap and unwrap these lists:

> instance Contains a (a :+: b) where
>     wrap         = L
>     unwrap (L x) = Just x
>     unwrap _     = Nothing

> instance Contains b (a :+: b) where
>     wrap         = R
>     unwrap (R x) = Just x
>     unwrap _     = Nothing

> instance Contains a s => Contains a (b :+: s) where
>     wrap         = R . wrap
>     unwrap (R x) = unwrap x
>     unwrap _     = Nothing

An event-aware monad
====================

Now, let's go back to our extensible, event-based framework. We'll assume all
clients of the framework can be implemented as a monad. We can abstract over
this monad, creating a typeclass for monads which can respond to an event of
type `e`:

> class (Functor m, Monad m) => MonadResponds e m where
>     fire :: e -> m ()

As you probably guessed, the `fire` method fires an event. We implement an
instance which is a `ReaderT`. This way, the underlying monad can access a
function which triggers an event:

> newtype RespondsT e m a = RespondsT
>     { unRespondsT :: ReaderT (e -> RespondsT e m ()) m a
>     } deriving (Applicative, Functor, Monad, MonadIO)

> runRespondsT :: RespondsT e m a -> (e -> RespondsT e m ()) -> m a
> runRespondsT (RespondsT r) e = runReaderT r e

By using this trigger, our `RespondsT` becomes an instance of `MonadResponds`.

> instance (Contains e s, Functor m, Monad m) =>
>         MonadResponds e (RespondsT s m) where
>     fire x = RespondsT $ ask >>= unRespondsT . ($ wrap x)

Now, all we need in order to write clients is some more syntactic sugar:

> client :: (Monad m, Contains e s) => (e -> m ()) -> s -> m ()
> client f = maybe (return ()) f . unwrap

A logging client
================

Let's start out by implementing a very simple logger as client for the
framework:

> data Log = Warn String | Info String

> logger :: (MonadIO m, Contains Log s) => s -> m ()
> logger = client $ \event -> liftIO $ putStrLn $ case event of
>     Warn s -> "[Warn]: " ++ s
>     Info s -> "[Info]: " ++ s

A ping client
=============

The logging client received events using `client`... let's see how we can
actually send events by writing an artificial ping-pong protocol. This client
uses features from the logger, so we can really compose clients by just listing
the required instances in the type signature (as is commonly done with monad
transformers), which is a pretty cool thing.

> data Ping = Ping Int | Pong Int

> ping :: (Contains Log s, Contains Ping s,
>           MonadResponds Log m, MonadResponds Ping m)
>      => s -> m ()
> ping = client $ \event -> case event of
>     Ping x -> fire (Pong x)
>     Pong x -> fire (Info $ "Received pong with token " ++ show x)

Actually running it
===================

If you've followed this blogpost until now, you probably want to see how we can,
in the end, combine a number of clients and run them.

To this end, we'll write a small utility function which combines a number of
handlers (our clients) by sequentially applying them to the same event).

> combine :: Monad m => [e -> m ()] -> e -> m ()
> combine handlers event = mapM_ ($ event) handlers

Now, let's use this to compose our clients. At this point, we're required to fix
the type for our client:

> type Features = Log :+: Ping

> testClient :: Features -> RespondsT Features IO ()
> testClient = combine [logger, ping]

And then we can write a program which uses these features:

> test :: RespondsT Features IO ()
> test = do
>     fire $ Warn "Starting the engines!"
>     fire $ Ping 100
>     fire $ Info "Engines has been started."
>     fire $ Ping 200

> main :: IO ()
> main = runRespondsT test testClient

I hope you've enjoyed this blogpost -- all criticism is welcome. If someone
feels like turning this into a proper library, you're also welcome.
