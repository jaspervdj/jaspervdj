---
title: "Zero-config MiniDLNA/ReadyMedia"
description: Quick and dirty way to stream video
tags: linux
---

# TL;DR:

You can use [ReadyMedia] without configuring it as a daemon.  Just `cd`
into any directory that has media files, and run this script:

<script src="https://gist.github.com/jaspervdj/64177596eb3aec4fe38ae117fc63db42.js"></script>

<noscript>

```bash
#!/bin/bash
set -o nounset -o errexit -o pipefail

# Create temporary locations for the configuration and data directories.
CONFIG="$(mktemp)"
DATADIR="$(mktemp -d)"

# Write the configuration to the temporary location.
echo "media_dir=$PWD" >>"$CONFIG"
echo "db_dir=$DATADIR" >>"$CONFIG"
echo "log_dir=$DATADIR" >>"$CONFIG"
echo 'force_sort_criteria=+upnp:class,+upnp:originalTrackNumber,+dc:title' >>"$CONFIG"
cat $CONFIG

# Make sure everything is cleaned up when this process is killed.
function cleanup {
  rm -r "$DATADIR"
  rm "$CONFIG"
}
trap cleanup exit

# Run minidlnad with the following flags:
#
#  -  `-f "$CONFIG"`: use the configuration we wrote.
#  -  `-f "$PWD/minidlnad.pid"`: store the `.pid` in the current directory.
#  -  `-d`: don't daemonize, we'll kill this when we're done.  This also
#     enabled "debug" mode; but I haven't seen any considerable slowdown
#     from this.
minidlnad -f "$CONFIG" -P "$PWD/minidlnad.pid" -d
```

</noscript>

Your television/phone/toaster should see the media server pop up within seconds.

# Motivation

In this day and age, there are literally thousands of ways to get video on to
your television screen, especially if you have a (somewhat) smart TV.  If not,
there is a plethora of devices that will let you stream from different sources.

For simply watching video files on my local disk, I used to just hook my laptop
up to the television using a simple HDMI cable, which always worked -- until the
HDMI port on my television broke.

I don't really want to get any of these devices, and I'm also not sure if I need
a newer television that [phones home].

In either case, most televisions that support any kind of networking will
also support the [DLNA] protocol.  For Linux, there's [ReadyMedia] (formerly
MiniDLNA), a relatively old project.  But despite lacking some maintenance,
it is pretty solid and reliable software.

By default, it runs as a daemon that stores a database of media.  This makes it
very cumbersome to use.  The database gets out of sync easily when you move
files around when it's not running.  The fact that it's a daemon means that
it could be running when you're working from a coffee place.  The daemon
needs to be managed through a file in `/etc/`.

I don't want to go through all that pain!  I just want to be able to fire it up
like you can get a quick HTTP server with just running `python -m http.server`
in any directory.  Then I can `cd` to whatever I want to watch and just run the
thing and then kill it.  I don't care about keeping this media database, since
scanning a single directory should be quick.

Well, it turns out you can do that fairly easily.  Just drop the script I linked
to at the top of this post in to your `$PATH` and you're good to go.

[DLNA]: https://www.dlna.org/
[phones home]: https://en.wikipedia.org/wiki/Phoning_home
[ReadyMedia]: http://minidlna.sourceforge.net/
