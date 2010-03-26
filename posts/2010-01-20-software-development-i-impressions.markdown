---
title: Software Development 1: My impressions
description: My thoughts on our first software development course at uni.
tags: ugent, code, design
---

## Why this post

This semester at [UGent](http://www.ugent.be/), the university I attend, we 
received a first course on Software Development. I had the feeling a lot of
students didn't like this course, so I decided to my thoughts on the matter
here.

## Software Development I

This course follow our two Programming courses, so all students are already more
than familiar with the basic concepts of programming - the second programming
course included writing a non-trivial GUI application. I put
[my solution](http://github.com/jaspervdj/Musique) on GitHub (after the
course was over).

The thing is, these two courses are taught in Java, and Software Development I
is taught in C/C++. And while you can translate most of these ideas from Java
to all other programming languages[^1], going from Java to C is not easy.

[^1]: All other programming languages, as long as they're object-oriented and
      imperative - or at least imperative.

So, what I first thought would be a course on design patterns and best
practices (I had this idea because of the name "Software Development 1" as
opposed to "Programming 3") turned out to be an introduction to C and C++.

## First Java, then C

Although I am not a big fan of Java, I have come to appreciate the language, and
I think it is, in fact, a very good "first language". It is, however, not easy
to go from Java to something like C. It is always harder to go more low-level,
and many students were confused by pointers, the fact that pointers can be
arrays, and all those _typical C_ things.

![Pointers cartoon]($root/images/2010-01-20-xkcd-pointers.png)

I had already had a little bit of experience with C/C++ before starting the
course (I wrote [jvgs](http://jvgs.sf.net/) this summer) - which, once again,
makes this post a little biased. But I thought the C part was pretty interesting
(the exercises at least, the theory was rather boring, but well, you can't have
everything, I suppose). I mean, C _is_ still used today, and to quote our Data
Structures & Algorithms professor:

> If you want, or need, something really fast - when all the constant factors
> __do__ matter - you would write it in C, even in 2009.

This aside, I had the feeling a lot of people still weren't very familiar with
C when we started on the C++ part of the course.

## Java and C, then C++

I know there is a lot of [C++ bashing](http://yosefk.com/c++fqa/) on the
internet, and when I read these things I sometimes think I must be one of the 5
people on the world who appreciate C++. So my problem here is not with C++ - I
just fail to see _why_ it is taught.

Let me explain: I see C++ as an object-oriented superset of C. Students know the
concept of object-orientation from Java, and they know about pointers and those
things from the C lessons: I do not think C++ adds any more _ideas_.

As far as I can see, the only new thing we saw with C++ was templates, but we
only used templates to write abstract data types - the primary use for Java
generics. Admittedly, we did not see generics in our Java courses, but we
_could have_.

## The Project

We also had to write a project in C++. There has been a lot of bullshit talk
and criticism about this project. I'm not going to repeat that (also because
I don't agree with most of it). You could work together with two people on the
project - [here is the solution](http://github.com/jaspervdj/AstroBot) by me
and [Javache](http://thinkjavache.be/blog/) (of course, this repository was
not publicly accessible during the project). In the end we had about 2500
lines of code, including comments, which is not a lot - especially not when
you're with two. I also really appreciate the fact the tutors tried to come up
with a project that is not "boring" - hey, it even included robots.

## Constructive feedback

Summary, I think C++ should not be taught, and more attention should be spent
on C, because:

- C++ does not add any _general_ programming knowledge - ideas we can use when
  learning other languages.
- A lot of students didn't know C well enough after the C part.
- It would be interesting to see a larger C project (now, we only had some
  rather basic C exercises).
- More time could be spent on C design and C best practices.

Well, that's my thoughts on this course. All feedback and criticism is welcome,
of course. I know the chance this would actually change approaches zero, but I
thought I'd give my opinion on it anyway.
