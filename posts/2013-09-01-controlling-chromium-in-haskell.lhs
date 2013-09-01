---
title: Controlling Chromium in Haskell
description: Driving Chromium using its built-in WebSockets server
tags: haskell
---

Introduction
============

<img src="/images/2013-09-01-chromium.png" alt="Chromium logo"
     style="float: right" />

The [chromium] browser has a built-in WebSockets server which can be used to
control it. I first heard about this through [an issue] with my WebSockets
project.

[chromium]: http://www.chromium.org/
[an issue]: https://github.com/jaspervdj/websockets/issues/39

Since I am currently rewriting the WebSockets library to have it use
[io-streams], I wanted to give this a try and made it into a blogpost. As a
sidenote -- the port is going great, io-streams is a great library and I managed
to solve a whole lot of issues in the library (mostly exception handling stuff).

[io-streams]: http://hackage.haskell.org/package/io-streams

Note that this code uses a yet unreleased version of the WebSockets library --
you can get it from [the github repo] though, if you want to: check out the
`io-streams` branch.

[the github repo]: https://github.com/jaspervdj/websockets

This file is written in literate Haskell so we'll have a few boilerplate
declarations and imports first:

> {-# LANGUAGE OverloadedStrings #-}
> module Main
>    ( main
>    ) where

> import           Control.Applicative  ((<$>))
> import           Control.Monad        (forever, mzero)
> import           Control.Monad.Trans  (liftIO)
> import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.=))
> import qualified Data.Aeson           as A
> import qualified Data.Map             as M
> import           Data.Maybe           (fromMaybe)
> import qualified Data.Text.IO         as T
> import qualified Data.Vector          as V
> import qualified Network.HTTP.Conduit as Http
> import qualified Network.URI          as Uri
> import qualified Network.WebSockets   as WS

Locating the WebSockets server
==============================

To enable the WebSockets server, chrome must be launched with the
`--remote-debugging-port` flag enabled:

    chromium --remote-debugging-port 9160

Now, in order to connect to connect to the built-in WebSockets server, we have
to know its URI and this requires some extra code. We'll first use [cURL] to
demonstrate this:

[cURL]: http://curl.haxx.se/

    $ curl localhost:9160/json
    [ {
       "description": "",
       ...
       "webSocketDebuggerUrl": "ws://localhost:9160/devtools/page/8937C189-5CED-8E34-E26E-A389641FE8FF"
    } ]

That `webSocketDebuggerUrl` is the one we want. Let's write some Haskell code to
automate obtaining it.

We create a datatype to hold this info. Currently, we're only interested in a
single field:

> data ChromiumPageInfo = ChromiumPageInfo
>     { chromiumDebuggerUrl :: String
>     } deriving (Show)

We will use [aeson] to parse the JSON. We need a `FromJSON` instance for our
datatype:

[aeson]: http://hackage.haskell.org/package/aeson

> instance FromJSON ChromiumPageInfo where
>     parseJSON (A.Object obj) =
>         ChromiumPageInfo <$> obj .: "webSocketDebuggerUrl"
>     parseJSON _              = mzero

The [http-conduit] library can be used to what we just did using `curl`:

[http-conduit]: http://hackage.haskell.org/package/http-conduit

> getChromiumPageInfo :: Int -> IO [ChromiumPageInfo]
> getChromiumPageInfo port = do
>     response <- Http.withManager $ \manager -> Http.httpLbs request manager
>     case A.decode (Http.responseBody response) of
>         Nothing -> error "getChromiumPageInfo: Parse error"
>         Just ci -> return ci
>   where
>     request = Http.def
>         { Http.host = "localhost"
>         , Http.port = port
>         , Http.path = "/json"
>         }

One remaining issue is that the JSON contains the WebSockets URL as a single
string, and the WebSockets library expects a (host, port, path) triple. Luckily
for us, the standard [network] library has a `Network.URI` module which makes
this task pretty simple:

[network]: http://hackage.haskell.org/package/network

> parseUri :: String -> (String, Int, String)
> parseUri uri = fromMaybe (error "parseUri: Invalid URI") $ do
>     u    <- Uri.parseURI uri
>     auth <- Uri.uriAuthority u
>     let port = case Uri.uriPort auth of (':' : str) -> read str; _ -> 80
>     return (Uri.uriRegName auth, port, Uri.uriPath u)

Once we are connected to Chromium, we will be sending commands to it. A simple
Haskell datatype can be used to model these commands:

> data Command = Command
>     { commandId     :: Int
>     , commandMethod :: String
>     , commandParams :: [(String, String)]
>     } deriving (Show)

We use the aeson library again here, to convert these commands into JSON data:

> instance ToJSON Command where
>     toJSON cmd = A.object
>         [ "id"     .= commandId cmd
>         , "method" .= commandMethod cmd
>         , "params" .= M.fromList (commandParams cmd)
>         ]

What's left is a simple main function to tie it all together.

> main :: IO ()
> main = do
>     (ci : _) <- getChromiumPageInfo 9160
>     let (host, port, path) = parseUri (chromiumDebuggerUrl ci)
>     WS.runClient host port path $ \conn -> do
>         -- Send an example command
>         WS.sendTextData conn $ A.encode $ Command
>             { commandId     = 1
>             , commandMethod = "Page.navigate"
>             , commandParams = [("url", "http://haskell.org")]
>             }
>
>         -- Print output to the screen
>         forever $ do
>             msg <- WS.receiveData conn
>             liftIO $ T.putStrLn msg

Conclusion
==========

This is a very simple example of what you can do with Haskell and Chromium, but
I think there are some pretty interesting opportunities to be found here. For
example, I wonder if it would be possible to create a simple [Selenium]-like
framework for web application testing in Haskell.

[Selenium]: http://docs.seleniumhq.org/

Thanks to Gilles J. for a quick proofread and Ilya Grigorik for this
[inspiring blogpost]!

[inspiring blogpost]: http://www.igvita.com/2012/04/09/driving-google-chrome-via-websocket-api/
