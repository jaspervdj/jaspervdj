---
title: Haskell Design Patterns: Service-Oriented Programming
description: A neat and simple way to build services in Haskell
tags: haskell
---

# Introduction

In this webscale age, we are all really familiar with using a service-oriented
approach to building big applications. These services commonly run over HTTP,
but they don't have to. Structuring code (within the same project) according to
this principle usually results in a neater organisation, and services can later
be separated to actually different processes.

The code idea of service-oriented programming is simple: separating a large
program out into multiple different services with clear responsibilities. A
lesser, but still important concern is that we want to have a separation between
the interface and the implementation of a service.

While I was already quite familiar with this technique, it was first shown to me
in the context of a large project by Simon Meier.

# A simple service

Let's look at a very simple service first. We are providing software for a
take-out restaurant, and we would like to build a service that stores whatever
people ordered.

If I am writing a service-oriented module, I tend to call the main type
`Handle`. This is designed for qualified import, e.g. it would be used as
`OrderLogger.Handle`, `Database.Handle`, ... throughout the rest of the code.

Furthermore, if we provide a `Handle` type, we should also provide a smart
constructor for it. Traditionally, we could go with `newHandle` (and optionally
`closeHandle`). People tend to prefer `withHandle` where possible though, since
it allows us to do proper resource allocation/deallocation in one place.

The following simple service simply stores the orders in memory.

`src/OrderLogger.hs`:

~~~~~{.haskell}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simple
    ( Order (..)
    , Handle
    , withHandle
    , log
    , count
    ) where

import           Control.Applicative ((<$>))
import           Data.IORef          (IORef, atomicModifyIORef, newIORef,
                                      readIORef)
import           Data.String         (IsString)
import qualified Data.Text           as T
import           Data.Time           (UTCTime, getCurrentTime)
import           Prelude             hiding (log)

newtype Order = Order T.Text
    deriving (Eq, IsString)

newtype Handle = Handle (IORef [(UTCTime, Order)])

withHandle
    :: (Handle -> IO a) -> IO a
withHandle f = do
    ref <- newIORef []
    f (Handle ref)

log :: Handle -> Order -> IO ()
log (Handle ref) o = do
    now <- getCurrentTime
    atomicModifyIORef ref (\l -> ((now, o) : l, ()))

count :: Handle -> Order -> IO Int
count (Handle ref) o =
    length . filter ((== o) . snd) <$> readIORef ref
~~~~~

# Separating interface and implementation

Keep in mind that having a service like this is perfectly fine and usable. In
the next part of the blogpost, we will explore how we can abstract over these
services, such that we can separate interface from implementation. If there is
only a single possible implementation, the above code is perfectly fine though:
I want to stress that premature abstraction is rarely a good idea.

Imagine, however, that we *do* want to separate interface from implementation.
A first step in that direction is using a more abstract `Handle` type:

`src/OrderLogger.hs`:

~~~~~{.haskell}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OrderLogger
    ( Order (..)
    , Handle (..)
    ) where

import           Data.String (IsString)
import qualified Data.Text   as T
import           Prelude     hiding (log)

newtype Order = Order T.Text
    deriving (Eq, IsString)

data Handle = Handle
    { log   :: Order -> IO ()
    , count :: Order -> IO Int
    }
~~~~~

It is important to note that the interface of the module above is exactly the
same as the implementation we first showed (minus `withHandle`). This is
precisely why we designed the functions in the first module to take the `Handle`
as a first argument.

This is significant. It means that even if we don't plan on decoupling services
right now, we are able to do so later without changing any code which uses the
service (it's pretty much a drop-in replacement).

We can rewrite the original simple implementation to confirm to our interface.
In order to do this, we only need to implement the missing `withHandle`, in
which we need to create a new `Handle` object.

`src/OrderLogger/Simple.hs`:

~~~~~{.haskell}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OrderLogger.Simple
    ( OrderLogger.Handle
    , withHandle
    ) where

import           Control.Applicative ((<$>))
import           Data.IORef          (atomicModifyIORef, newIORef, readIORef)
import           Data.Time           (getCurrentTime)
import qualified OrderLogger         as OrderLogger
import           Prelude             hiding (log)

withHandle
    :: (OrderLogger.Handle -> IO a) -> IO a
withHandle f = do
    ref <- newIORef []
    f OrderLogger.Handle
        { OrderLogger.log   = \o -> do
            now <- getCurrentTime
            atomicModifyIORef ref (\l -> ((now, o) : l, ()))
        , OrderLogger.count = \o ->
            length . filter ((== o) . snd) <$> readIORef ref
        }
~~~~~

Using such a service is very straighforward.

`src/Main.hs`:

~~~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}
import qualified OrderLogger        as OrderLogger
import qualified OrderLogger.Simple as OrderLogger.Simple

main :: IO ()
main = OrderLogger.Simple.withHandle $ \h -> do
    OrderLogger.log h "Nachos"
    OrderLogger.log h "Nachos"
    OrderLogger.log h "Tacos"
    OrderLogger.log h "Nachos"
    OrderLogger.log h "Corona"
    print =<< OrderLogger.count h "Nachos"
~~~~~

# An improved service

## Configuration

Let's create a better `OrderLogger.Handle` now. We want to properly persist
orders in a database. We will assume that the database has already been set up,
using e.g.:

~~~~~{.haskell}
CREATE TABLE orders (
    time TIMESTAMP NOT NULL,
    item TEXT      NOT NULL
);
~~~~~

Of course, that doesn't tell us which database we should connect to. We can
specify that in a configuration file:

`services.conf`:

~~~~~{.conf}
order-logger {
    user     = "jaspervdj"
    password = ""
    database = "services"
}
~~~~~

We can easily load this using the excellent [configurator] library (note the use
of the [RecordWildCards](TODO) language extension). Just as we designed
`Handle` for qualified import, we can design `Config` as well:

~~~~~{.haskell}
data Config = Config
    { cUser     :: !T.Text
    , cPassword :: !T.Text
    , cDatabase :: !T.Text
    }

parseConfig :: C.Config -> IO Config
parseConfig c = do
    cUser     <- C.require c "user"
    cPassword <- C.require c "password"
    cDatabase <- C.require c "database"
    return Config {..}
~~~~~

In fact -- most of the services in a real-world application would probably have
both a `Config` as well as a `Handle`.

## Connection pool

We will use another library in the code in addition to postgresql-simple:
namely, [resource-pool](TODO). This is one of those fairly unknown Haskell
libraries that everyone ends up reimplementing without knowing it exists, I
guess.

However, it does exist, and it is really great. With only three core functions
from that library, we will be able to set up a pool containing connections to
our database.

## Tying it up

Now for the full code. Everything should be relatively easy to understand now.

`src/OrderLogger/Postgres.hs`:

~~~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module OrderLogger.Postgres
    ( Config (..)
    , parseConfig

    , OrderLogger.Handle
    , withHandle
    ) where

import           Control.Exception          (bracket)
import           Control.Monad              (void)
import qualified Data.Configurator          as C
import qualified Data.Configurator.Types    as C
import qualified Data.Pool                  as Pool
import qualified Data.Text                  as T
import           Data.Time                  (getCurrentTime)
import qualified Database.PostgreSQL.Simple as Postgres
import qualified OrderLogger                as OrderLogger
import           Prelude                    hiding (log)

data Config = Config
    { cUser     :: !T.Text
    , cPassword :: !T.Text
    , cDatabase :: !T.Text
    }

parseConfig :: C.Config -> IO Config
parseConfig c = do
    cUser     <- C.require c "user"
    cPassword <- C.require c "password"
    cDatabase <- C.require c "database"
    return Config {..}

type PostgresHandle = Pool.Pool Postgres.Connection

withPostgresHandle :: Config -> (PostgresHandle -> IO a) -> IO a
withPostgresHandle config f = bracket
    (Pool.createPool
        (Postgres.connect connectionInfo)  -- How to create a resource
        Postgres.close                     -- How to close a resource
        4                                  -- 4 stripes
        10                                 -- 10 seconds resource lifetime
        4)                                 -- 4 connections per stripe
    Pool.destroyAllResources
    f
  where
    connectionInfo = Postgres.defaultConnectInfo
        { Postgres.connectUser     = T.unpack $ cUser     config
        , Postgres.connectPassword = T.unpack $ cPassword config
        , Postgres.connectDatabase = T.unpack $ cDatabase config
        }

mkHandle :: PostgresHandle -> OrderLogger.Handle
mkHandle h = OrderLogger.Handle
    { OrderLogger.log   = log h
    , OrderLogger.count = count h
    }

withHandle :: Config -> (OrderLogger.Handle -> IO a) -> IO a
withHandle config f = withPostgresHandle config (f . mkHandle)

log :: PostgresHandle -> OrderLogger.Order -> IO ()
log h (OrderLogger.Order o) = Pool.withResource h $ \c -> do
    now <- getCurrentTime
    void $ Postgres.execute c
        "INSERT INTO orders (time, item) VALUES (?, ?)"
        (now, o)

count :: PostgresHandle -> OrderLogger.Order -> IO Int
count h (OrderLogger.Order o) = Pool.withResource h $ \c -> do
    rows <- Postgres.query c
        "SELECT COUNT(*) FROM orders WHERE item = ?"
        (Postgres.Only o)
    case rows of
        [Postgres.Only i] -> return i
        _                 -> fail "count: expected query result"
~~~~~

That would be an example of code I expect to find in a real-world Haskell
codebase. One thing is missing though -- the nasty error handling using `fail`.
I hope I can write about error handling in a next blogpost.

# Conclusion

Apart from having shown how to use configuration files, and how to use
`resource-pool`, we've writing a standalone example of a service.

Making a clear separation between interface and implementation like this has
multiple advantages, namely:

- plug in and replace services without having to change other components;
- you are forced to think about the interface as a separate concept -- the
  interface is not just a sideproduct of the implementation;
- compilation speed should increase as well, since most other components depend
  only on the interface of a service, not the implementation;
- a standard `Config`/`Handle` scheme makes it easy to see what is going on.
