---
title: Memory leak hunting in Java
description: A bit about a recent memory leak hunt in a university project
tags: java, code, ugent
---

## What is this

This semester at [UGent], we're following a course "Software-Development II".
It's a Java-based course, were we learned a bit about design patterns, nothing
really special. The fun part was that this course included a project. I'm not a
big Java fan, but I do think it's not a bad language for cross-platform
event-based GUI programming.

[UGent]: http://ugent.be

Our project consisted of creating multimedia, peer-to-peer chat application. The
"multimedia" part consisted of the fact that the application should find
images (nothing fancy, just google image or Flickr search) relevant to the
conversation subject and show these to the users. The user can then recommend
images to the conversation partner.

![Screenshot of the application]($root/images/2010-05-20-ch9k.png)

## The problem

Two weeks or so ago, the project was getting close to completion, and we were
quite excited about this. However, at a certain point, we were testing the
program slightly longer than usual, we suddenly got the not-so-awesome
`java.lang.OutOfMemoryException`. [Yay]!

[Yay]: http://twitter.com/jaspervdj/status/13795203285

![The path to the GC roots]($root/images/2010-05-20-gc-roots.png)
