---
title: Memory leak hunting in Java
description: A bit about a recent memory leak hunt in a university project
tags: ugent
---

# What is this

This semester at [UGent], we're following a course "Software-Development II".
It's a Java-based course, were we learned a bit about design patterns, nothing
really special. The fun part was that this course included a project. I'm not a
big Java fan, but I do think it's not a bad language for cross-platform
event-based GUI programming.

[UGent]: http://ugent.be

Our project consisted of creating multimedia, peer-to-peer chat application. The
"multimedia" part consisted of the fact that the application should find
images (nothing fancy, just Google Image or Flickr search) relevant to the
conversation subject and show these to the users. The user can then recommend
images to the conversation partner.

![Screenshot of the application](/images/2010-05-20-ch9k.png)

# The problem

Two weeks or so ago, the project was getting close to completion, and we were
quite excited about this. However, at a certain point, we were testing the
program slightly longer than usual, we suddenly got the not-so-awesome
`java.lang.OutOfMemoryException`. [Yay]!

[Yay]: http://twitter.com/jaspervdj/status/13795203285

I started a bit of bug hunting with [nudded]. Using [jconsole], we saw that the
memory usage rose linearly -- which is, erm, not really good. Our University
tends to push [netbeans] as IDE for Java development[^1], so we tried using it's
profiler on our project, but we didn't really get any results.

[nudded]: http://twitter.com/nudded
[jconsole]: http://java.sun.com/developer/technicalArticles/J2SE/jconsole.html
[netbeans]: http://netbeans.org/
[^1]: No worries, I still use vim.

Because of the plugin-based nature of the project, it was quickly made out that
the memory leak was located in the image recommendation system (read: it didn't
occur when we disabled this).

I was the author of it, and I thought the possibility of a memory leak was real:
the images were kept in different lists and sets, for convenience and
performance reasons.

# The solution

However, after a lot of frustrations, it turned out to be something completely
different. We were using Java serialization for our communication, and we were
sending the images over the network using an `ObjectOutputStream`.

I discovered eclipse's [memory analyzer tool]. It's not a great piece of
software, and quite unstable, but it has one particulary cool feature: the
possibility to trace the path to the GC roots for an object.

[memory analyzer tool]: http://www.eclipse.org/mat/

![The path to the GC roots](/images/2010-05-20-gc-roots.png)

Wait, the image is kept by the `ObjectOutputStream`? Our class looked a bit like:

~~~~~{.java}
public class ProvidedImage implements Serializable {
    /**
     * URL of the image.
     */
    private URL url;

    /**
     * Actual image. We use transient here so we don't send the (possible large)
     * image over the network, the receiver can retrieve it using the URL.
     */
    private transient Image image;

    /* More stuff ... */
}
~~~~~

Wait? The `Image` is `transient`? How the heck can we have a memory leak here?
Well, it turns out that Java serialization tries to be smarter than it should
be.

Imagine the following scenario: we send objects __a__, __b__ to the
`ObjectOutputStream`. We still have an object __c__, which has a reference to
__a__.  What will happen when we send `c`? In particular, what will the
reference to __a__ look like?

The answer is that the `ObjectOutputStream` "remembers" the object __a__: it
keeps a table of previously serialized objects. The fact that the image is
`transient` does not matter here, because there's still a reference to the
`Image`.

Possible solutions included:

1. Dropping the `Image` member from the class, and storing that elsewhere.
2. Using XML or JSON (probably the best solution, but we didn't have to time to
   throw half of the project around).
3. Closing or resetting the `ObjectOutputStream` regularly. But when, and why?

We chose (1), because (2) was impossible and (3) looked a little dirty. And
that, as they say, is that.
