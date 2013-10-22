---
title: WebSockets 0.8
description: New major release of the Haskell WebSockets library
tags: haskell
---

Introduction
============

Today, I released version 0.8 of the [websockets] library. Important changes
include:

[websockets]: http://jaspervdj.be/websockets

- The underlying IO library has been changed from [enumerator] to [io-streams];
- The API has been redesigned to work with a `Connection` datatype and plain
  `IO` instead of a custom `Monad`;
- Support for [deprecated] protocols has been removed, simplifying the API.

[enumerator]: http://hackage.haskell.org/package/enumerator
[io-streams]: http://hackage.haskell.org/package/io-streams
[deprecated]: http://hacks.mozilla.org/2010/12/websockets-disabled-in-firefox-4/

Since this all implies means a **huge** simplification of the API, updating
should be a pleasant experience -- but please let me know if you run into
trouble.

A fun example
=============

Let us write a fun example using the new API. I implemented a super simple
browser console, running a shell on the server and communicating with the
browser using WebSockets.

![A WebSockets-based browser console](/images/2013-10-22-websockets-console.png)

Obviously this is quite insecure, so do not run this on your own server without
adding proper authentication!

This blogpost is written in literate Haskell. However, it needs some HTML and
JavaScript as well. In order to run this, run `server.hs` from [this directory].

[this directory]: https://github.com/jaspervdj/websockets-snap/tree/master/example

As always, we start with a bunch of imports. We are using the [snap backend]
here. I still need to write support for [wai] (or Warp), hopefully I can do that
soon (or [contact] me if you would like to hack on this).

[snap backend]: http://hackage.haskell.org/package/websockets-snap
[wai]: http://hackage.haskell.org/package/wai
[contact]: /contact.html

> {-# LANGUAGE OverloadedStrings #-}
> module Main where

> import           Control.Concurrent      (forkIO)
> import           Control.Exception       (fromException, handle, throw)
> import           Control.Monad           (forever, unless)
> import qualified Data.ByteString         as B
> import qualified Data.ByteString.Char8   as BC
> import qualified Network.WebSockets      as WS
> import qualified Network.WebSockets.Snap as WS
> import           Snap.Core               (Snap)
> import qualified Snap.Core               as Snap
> import qualified Snap.Http.Server        as Snap
> import qualified Snap.Util.FileServe     as Snap
> import qualified System.IO               as IO
> import qualified System.Process          as Process

The main Snap application only supports four routes. The index page, two
additional resources and our `console` handler, which we will look at in detail
next.

> app :: Snap ()
> app = Snap.route
>     [ ("",               Snap.ifTop $ Snap.serveFile "console.html")
>     , ("console.js",     Snap.serveFile "console.js")
>     , ("style.css",      Snap.serveFile "style.css")
>     , ("console/:shell", console)
>     ]

The browser will make a WebSockets connection to `/console/:shell`. The `shell`
argument determines the command we will run. Obvious examples are
`/console/zsh`, `/console/bash`, and `/console/ghci`.

By using `WS.runWebSocketsSnap`, the HTTP connection is passed from Snap to the
WebSockets library, which runs the `consoleApp`.

> console :: Snap ()
> console = do
>     Just shell <- Snap.getParam "shell"
>     WS.runWebSocketsSnap $ consoleApp $ BC.unpack shell

Here, we have `consoleApp`, the actual WebSockets application. Note that
`WS.ServerApp` is just a type synonym to `WS.PendingConnection -> IO ()`.

We start out by running the `shell` command, obtaining handles to the `stdin`,
`stdout` and `stderr` streams. We also accept the pending connection regardless
of what's in there: proper authentication would not be a bad idea.

> consoleApp :: String -> WS.ServerApp
> consoleApp shell pending = do
>     (stdin, stdout, stderr, phandle) <- Process.runInteractiveCommand shell
>     conn                             <- WS.acceptRequest pending

Once the connection is accepted, we fork threads to stream data:

- We send everything that appears on `stdout` to the browser;
- We do the same for `stderr`;
- We send every message coming from the browser to `stdin`.

The `copyHandleToConn` and `copyConnToHandle` functions are defined later in
this file.

>     _ <- forkIO $ copyHandleToConn stdout conn
>     _ <- forkIO $ copyHandleToConn stderr conn
>     _ <- forkIO $ copyConnToHandle conn stdin

Now that our input/output is set up, we wait for the shell to finish. Once our
`WS.ServerApp` completes, the WebSockets connection will be closed
automatically.

>     exitCode <- Process.waitForProcess phandle
>     putStrLn $ "consoleApp ended: " ++ show exitCode

The first utility function is a loop reading from a plain `IO.Handle`, and using
`WS.sendTextData` to send messages to the browser.

> copyHandleToConn :: IO.Handle -> WS.Connection -> IO ()
> copyHandleToConn h c = do
>     bs <- B.hGetSome h 1024
>     unless (B.null bs) $ do
>         putStrLn $ "> " ++ show bs
>         WS.sendTextData c bs
>         copyHandleToConn h c

The second utility function does the reverse. It uses `WS.receiveData` to wait
for and receive messages from the browser. It writes these to the provided
`IO.Handle`. We also watch for the `WS.ConnectionClosed` exception, so we can
cleanly close the handle.

> copyConnToHandle :: WS.Connection -> IO.Handle -> IO ()
> copyConnToHandle c h = handle close $ forever $ do
>     bs <- WS.receiveData c
>     putStrLn $ "< " ++ show bs
>     B.hPutStr h bs
>     IO.hFlush h
>   where
>     close e = case fromException e of
>         Just WS.ConnectionClosed -> IO.hClose h
>         Nothing                  -> throw e

What is left is a super-simple `main` function to serve our Snap application
over HTTP:

> main :: IO ()
> main = Snap.httpServe config app
>   where
>     config =
>         Snap.setErrorLog  Snap.ConfigNoLog $
>         Snap.setAccessLog Snap.ConfigNoLog $
>         Snap.defaultConfig

Appendix: IO libraries
======================

Recently, the [war on IO libraries] started again. Since this blogpost is
somewhat about a port between two IO libraries, some of you might think I have
an enlightened opinion on this subject.

[war on IO libraries]: http://www.yesodweb.com/blog/2013/10/pipes-resource-problems

I do not. I can see how `pipes` and `conduit` make creating and composing IO
streams easier, but I do not have a clue about which one is easier to use, or
more formally correct.

The only reason I chose to use `io-streams` is out of practical considerations.
I think the available IO libraries can be classified in two main groups:

- `pipes`, `conduit`, `enumerator`: provide high-level easy-to-use combinators
  for doing IO. This is a great way to rapidly write great *applications*.
- `System.IO`, `io-streams`: provide lower-level access to IO resources. I think
  these are great to write *libraries*, since libraries built upon these IO
  libraries can be easily integrated into *any application*.

I have had some trouble in the past integrating the `enumerator`-based
WebSockets library with `conduit`-based Warp. I think it might also be tricky to
use a `pipes`-based library in a `conduit`-based application, and vice versa.

However, by building libraries on top of `io-streams` (or `System.IO`, but I
think `io-streams` is more convenient), I get libraries that are easy to
integrate almost everywhere.
