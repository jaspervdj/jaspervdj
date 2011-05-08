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

Although pretty efficient, a completely automated system would be a nice-to-have
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
- router: to create a more complicated network setup (see later)
- receiver: sending all received bluetooth data to a central computing node

[Voyage Linux]: http://linux.voyage.hk/
[Gyrid]: http://github.com/Rulus/Gyrid

### Relay batons

TODO

TODO: picture of a node (preferably in action)

### Network setup

TODO

TODO: silly drawing of a circle with some nodes

### count-von-count

TODO

TODO: drawing to illustrate the linear regression used

### dr.beaker

TODO

TODO: nag about the incompetence of blackskad

Conclusion
==========

TODO

It's a hardware problem, ideas for next year.
