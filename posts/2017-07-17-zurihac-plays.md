---
title: 'ZuriHac plays'
description: 'Collaborative gaming as fun Hackathon project'
tags: haskell
---

Introduction
============

This is a small write-up of a fun Haskell project that [Andras Slemmer],
[Francesco Mazzoli] and I worked on during [ZuriHac] 2017.

[ZuriHac]: https://zurihac.info/
[Andras Slemmer]: https://github.com/exFalso
[Francesco Mazzoli]: http://mazzo.li/

I work with Haskell professionally, and it can be hard to motivate myself to
work on similar stuff during Hackathons.  This year I also had a large part in
organising the Hackathon, so there was little room to take on a serious project.

This is why I joined in the fun of of creating a deliberately silly thing during
this Hackathon (aside from still trying to help people out as much as possible).

This year, we decided to implement something in the style of [_Twitch Plays
Pokémon_](https://en.wikipedia.org/wiki/Twitch_Plays_Pok%C3%A9mon).  Rather than
picking a slow, turn-based game such as Pokémon, however, we wanted to try the
same thing for a fast game, such as a platformer.

For the impatient, here is a quick preview:

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Haskell
programmers: amazing programmers, terrible at coordinating a game of Super
Mario. <a href="https://twitter.com/hashtag/ZuriHac?src=hash">#ZuriHac</a> <a
href="https://t.co/74ts3WF5Eu">pic.twitter.com/74ts3WF5Eu</a></p>&mdash;
cocharles (@acid2) <a
href="https://twitter.com/acid2/status/873907429267972096">June 11,
2017</a></blockquote>

The core design
===============

The core design question of the project is how to handle keypresses and
aggregate them when you have many concurrent users.  _Twitch Plays Pokémon_
solved this problem in two distinct modes:

1. Anarchy mode (the default): any user keypress is sent directly to the game.

2. Democracy mode: there is a voting window, after which the most popular user
   keypress is selected and sent to the game.

With a majority vote, players could switch between the modes.

There are a bunch of reasons why this does not work great for faster games:

- Action games typically need you to hold a key for a certain amount of time,
  rather than just pressing and then releasing the key (e.g. Mario jumps higher
  if you hold `jump` for longer).

- There's little time to switch between modes in a fast-paced game.

- Many games require you to press more than one key at the same time (e.g.
  `jump` and `right` in Mario).

- ...

We solved this by putting a key voting algorithm in place to aggregate the
key events from the users.  We think our algorithm should work well with most
games, if the parameters are tweaked a bit.

First, imagine that we are looking at every key independently.  For a given key,
we might receive the following input from users, where a block means that the key
is pressed:

![](/images/2017-07-17-key-voting-01.png)

We divide the time in sample intervals.  The length of the sample interval
can be tweaked per game.  Let's imagine it is 10ms for our example.

![](/images/2017-07-17-key-voting-02.png)

Every key press is expanded to match the sample interval first.  This gives us
something like:

![](/images/2017-07-17-key-voting-03.png)

We can aggregate them according to a treshold.  This is another parameter that
can be tweaked per game.  In our example, we can set this treshold to 0.5.  This
means that 50% of users must be pressing a key before we consider it pressed.
Concretely, for our 3 users, that means that at least two people must be
pressing the key.  This gives us the following aggregates:

![](/images/2017-07-17-key-voting-04.png)

After we've aggregated the key presses, we can send the result to the game.
It's important to note that this happens one sample interval _after_ the actual
user keypresses, since you are not easily able to make any conclusions before
the interval has ended.  This adds some latency but we didn't find this a
problem in practice for the games we tried.

![](/images/2017-07-17-key-voting-05.png)

Apart from that, we added to more complications to make the experience smoother:

1. We look at all keys independently using the algorithm above, but before we
   decide on the final output, we take _key groups_ into account.

    In _Super Mario World_, if you press `left` and `right` at the same time,
    Mario does not move.  That is a problem: if the treshold i set to 0.2, 30%
    of people are pressing `left`, and 40% of people are pressing `right`, we
    would expect Mario to move right.  However, using our naive algorithm,
    nothing happens.

    This is why we added key groups.  A key group is a set of keys out of which
    at most one can be pressed.  For example, `{left, right}` forms such a key
    group for Mario.  We select the most popular key if there are multiple
    candidates within a group (`right` in the example).

2. There is a timeout timer for activity per user.  If the user does not press a
   key in a while, he is considered inactive, and this user is not counted
   towards the total amount of users.  This prevents people from loading the
   page up but not participating from influencing the game too much.

The setup
=========

That takes care of the key logic component, so now let's look at the stack.

![](/images/2017-07-17-setup.png)

It's all pretty self-explanatory:

- Users open an HTML page which contains a JavaScript keylogger.  We send the
  `KeyPress` and `KeyRelease` events to the server.  On mobile, people can use a
  touchscreen interface which sends the same events.

- The server runs the key voting algorithm we discussed before and sends the
  aggregated `KeyPress` and `KeyRelease` events to any connected sinks.

- The main sink we implemented just executes the events using the
  `XTestFakeInput` call from [xtest].  In our case, the sink ran on the same
  machine as the server (my laptop).

[xtest]: https://www.x.org/releases/X11R7.7/doc/xextproto/xtest.html#Server_Requests

We played _Super Mario World_ with around 60 people on local Wi-Fi.  We required
40% of people to press a key for the voting, in 10ms sampling windows.  The
system performed very smoothly, although the same cannot be said about the
collaboration between users.

Thanks for joining in the fun!  The code for our project can be found
[here](https://github.com/bitonic/zurihac-plays).

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Here is
the video I actually meant to upload but twitter cropped. <a
href="https://twitter.com/hashtag/zuriHac?src=hash">#zuriHac</a> plays Mario <a
href="https://t.co/Ku76aH0OpN">pic.twitter.com/Ku76aH0OpN</a></p>&mdash;
cocharles (@acid2) <a
href="https://twitter.com/acid2/status/874170993778073600">June 12,
2017</a></blockquote>

<blockquote class="twitter-tweet" data-lang="en"><p lang="en"
dir="ltr">Collaborative Mario at <a
href="https://twitter.com/ZuriHac">@ZuriHac</a> <a
href="https://twitter.com/hashtag/zurihac2017?src=hash">#zurihac2017</a> fun!
:-) <a href="https://t.co/6h3ulZRoZH">pic.twitter.com/6h3ulZRoZH</a></p>&mdash;
Simon Thompson (@thompson_si) <a
href="https://twitter.com/thompson_si/status/873906086444752896">June 11,
2017</a></blockquote>

<!-- I'm very sorry for breaking the no-JavaScript rule on my website. -->
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>
