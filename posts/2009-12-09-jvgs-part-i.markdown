---
title: What the hell is jvgs -- Part I
description: Every project has a story: how and why jvgs originally started
tags: jvgs, gamedev
---

This post is about [jvgs](http://jvgs.sf.net/), a game I wrote.

## Java and Swing

Back in June, I was searching for a project to work on. At that time, I was
reading a lot about
[automatic content generation in games](http://en.wikipedia.org/wiki/Procedural_generation).
I became interested and tried to find a good concept to base a game on.

I am a regular reader of [XKCD](http://xkcd.com) and I like the drawing style a
lot. A _sketched_ is pretty cool for a game, so I started fooling around with
[noise](http://en.wikipedia.org/wiki/Perlin_noise). I first wrote an initial
test in Java using swing.

![Screenshot of the Java test]($root/images/2009-12-09-jvgs-part-i-swing.png)

## C++ and OpenGL

Obviously, swing is __not__ very well suited for games that require some
graphical performance. So I ported it to C++/OpenGL/SDL (a combo I am very
familiar with), and after some more coding, I ended up with something like
this:

![A prototype of the OpenGL test]($root/images/2009-12-09-jvgs-part-i-map.jpg)

As you can see, at that time the idea would've been to develop some strategy
game. As you can see, I was able to draw hills and forests. The problem with
this was that every entity was drawn in a custom way (basically hard-coded). I
needed some kind of image format that would allow me to still have a
sketchy look and feel, so I implemented a subset of
[svg](http://www.w3.org/Graphics/SVG/).

## A concept please

After that, I still needed a game concept. That's when I found my way to
[this beauty](http://giantsparrow.com/games/swan/). This game had some
deeper meaning, and that's basically what I wanted in my game as well. Soon
afterward, I had already written some code for a collision system, so I settled
with a platform game.

I don't know a lot of games based upon poetry, so that is the direction I
chose. I started coding more, writing poetry, thinking of more ideas...

![Screenshot of jvgs gameplay]($root/images/2009-12-09-jvgs-part-i-gameplay.png)

I didn't know how the game would end until I was coding the last level. This
might be why the story seems vague or non-existant, and I can confirm that.
It's more a random stream of thoughts, it's not a story.

This post described some of the ideas behind jvgs. The next post about the
project will be more technical and explain some of the details. Your feedback
is, as always, very welcome.

Your most humble and obedient servant,
Jasper Van der Jeugt
