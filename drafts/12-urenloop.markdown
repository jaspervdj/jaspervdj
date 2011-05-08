---
title: Counting laps using bluetooth dongle detection on the 12 urenloop
description: It's a hardware problem
tags: code, ugent
---

The [12 urenloop] is a yearly contest held at [Ghent University]. The student
clubs compete in a 12-hour-long relay race to run as much laps as possible. Each
of the 14 teams this year had a baton assigned, so they can only have one runner
at any time.

This event is not all about the running -- it's become more of a festival, with
lots of things to do and see (I hope I can convince you to check it out if
you're based in Ghent) -- but I will focus on the running here, and
more specifically: the system used to count the laps.

[12 urenloop]: http://www.12urenloop.be/
[Ghent University]: http://www.ugent.be/

TODO: picture of 12 urenloop

The manual way
==============

Lap counting used to be done in a manual way -- people worked in shifts, with
two people counting laps at the same time. Simple touchscreens were used, so
they basically just sat next to the circuit, looked at the runners that passed
and touched the corresponding buttons on the screen.

Although pretty efficient, a completely automated system would be nice-to-have
for several reasons:

- less mistakes are possible (provided it's a *good* system);
- fewer people are needed (provided it doesn't need constant monitoring);
- the data can be used for several real-time visualisations.

So, [Zeus WPI], the computer science club I am a committee member of, decided to
take on this challenge.

[Zeus WPI]: http://zeus.ugent.be/

The hardware
============

### Bluetooth

We decided to attach bluetooth dongles to the relay batons. I'm now pretty
confident this was a good choice. The other option was the more obvious [RFID],
but the main problem here was that RFID hardware is ridiculously expensive.
Besides, we already had pretty awesome embedded devices we could use as
bluetooth receivers.

[RFID]: http://en.wikipedia.org/wiki/Radio-frequency_identification

### Gyrid

These bluetooth receivers were borrowed from the [CartoGIS], a research group
which (among other things) studies technology to track people on events
(e.g. festivals) using bluetooth receivers.

[CartoGIS]: http://geoweb.ugent.be/cartogis/

TODO: picture of a node (preferably in action)

The receivers run a custom build of [Voyage Linux] created to run the [Gyrid]
service. What does this mean for us? We get simple, robust nodes we can use as:

- linux node: we can simply SSH to them and set them up
- switch: to create a more complicated network setup (see later)
- receiver: sending all received bluetooth data to a central computing node

[Voyage Linux]: http://linux.voyage.hk/
[Gyrid]: http://github.com/Rulus/Gyrid

### Relay batons

TODO

TODO: picture of a node (preferably in action)

### Network setup

The problem here was that we only could put cables *around* the circuit, we
couldn't cut right through to the other side of the circuit. This means the
commonly used [Star network] was impossible (well, theoretically it was
possible, but we would need *a lot* of cables).

[Star network]: http://en.wikipedia.org/wiki/Star_network

Instead, [Jens], [Pieter] and [Toon] created an awesome ring-based network, in
which each node also acts as a switch (using [bridging-utils]). Then, the
[Spanning Tree Protocol] is used to determine an optimal network layout, closing
one link in the circle to create a tree.

[Jens]: http://twitter.com/jenstimmerman
[Pieter]: http://thinkjavache.be/
[Toon]: http://twitter.com/nudded
[bridging-utils]: http://www.linuxfoundation.org/collaborate/workgroups/networking/bridge
[Spanning Tree Protocol]: http://en.wikipedia.org/wiki/Spanning_Tree_Protocol

This means we didn't have to use *too much* cables, and still had the property
that one link could go down (physically) without bringing down any nodes: in
this case, another tree would be chosen. And if two contiguous links went down,
we would only lose one node (obviously, the one in between those two links)!

TODO: silly drawing of a circle with some nodes

### count-von-count

Now, I will elaborate on the software which interpolates the data received from
the gyrid nodes in order to count laps [^1]. `count-von-count` is a robust
system written in the [Haskell] programming language.

[Haskell]: http://haskell.org/

[^1]: Because the author of this blogpost is also the author of
      `count-von-count`, this component is explained in a little more detail.

At this point, we have a central node which receives 4-tuples from the gyrid
nodes:

    (Timestamp, Mac receiver, Mac relay baton, RSSI value)

After some initial tests, we concluded the [RSSI] value was not too useful for
us. Later, we did use it to determine if a signal was strong enough (i.e. RSSI
above a certain treshold), and then we discarded the RSSI value. This leaves us
with a triplet:

[RSSI]: http://en.wikipedia.org/wiki/Received_signal_strength_indication

    (Timestamp, Mac receiver, Mac relay baton)

We do the calculations separately for each team -- only we work with relay
batons instead of teams. This means that we get, for every team:

    (Timestamp, Mac receiver)

We also ([hopefully](http://bash.org/?5273)) know the location of our gyrid
nodes, which means we can again map our data to something more simple:

    (Timestamp, Position)

TODO: drawing to illustrate the linear regression used

### dr.beaker

TODO

TODO: nag about the incompetence of blackskad

Conclusion
==========

TODO

It's a hardware problem, ideas for next year.
