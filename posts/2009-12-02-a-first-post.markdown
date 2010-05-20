---
title: A first post
description: A post describing the why and the how of the technical setup of this blog
tags: hakyll, haskell
---

A first post describing the technical setup of this blog, for that is
what technical blogs usualy do. I started looking around, and these
were my best options:

- [Wordpress](http://wordpress.org/) is probably the most common blog
  software. I didn't really like it, because it's written in php, and
  because I recently became interested in static site generators.
- [nanoc](http://nanoc.stoneship.org), written by someone I know, seemed
  more appropriate. The downside was that it requires some Ruby
  knowledge, which I am currently lacking.
- [yst](http://github.com/jgm/yst) is a similar static site generator,
  mostly based on data files. I didn't really like it's configuration
  system, and the next item looked better:
- [jekyll](http://github.com/mojombo/jekyll) is also written in Ruby, but
  it seems more high-level and blog-ready (which is a good thing, you know,
  with my non-existant Ruby skills...). However, when I tried it out I
  ran across a certain bug, and I started googling. It turned out jekyll
  is not perfectly compatible with Ruby 1.9. Yeah, I'll repeat that: it's
  not compatible with Ruby 1.9. I didn't feel like downgrading any
  packages, so I went for the last option:
- Write my own system. I named it hakyll, after jekyll. It is mostly based
  upon jekyll, altough a lot simpler.

The current setup is really simple. Inspired by
[my favorite window manager](http://xmonad.org), it uses a Haskell file as main
configuration tool. I'll now explain the system a little by showing some random
pieces of code.

~~~~~{.haskell}
main = do
    staticDirectory "images"
    staticDirectory "css"
    staticDirectory "js"
~~~~~

See what I did there? I declared some static directories. These will just be
copied directly when I generate the site.

~~~~~{.haskell}
postPaths <- liftM (L.reverse . L.sort) $ getRecursiveContents "posts"
~~~~~

Here, I take all posts paths from the `posts` directory. I sort them and then
I reverse them, so the most recent posts will come first. Now I'm going to
render all posts:

~~~~~{.haskell}
sequence (map readPage postPaths) >>=
    mapM (renderAndWrite "templates/default.html")
~~~~~

For those who do not know Haskell, I just read all post pages using a map, and
then I mapped the result again, so all posts were written using the
`templates/default.html` template. The templates are very simple, an example
could be

~~~~{.html}
<head>
  <title> $title </title>
</head>
<body>
  $body
</body>
~~~~

Were all $identifiers get replaced with items from a map. Another cool feature
is that the amazing [Pandoc](http://johnmacfarlane.net/pandoc/) library is used
for converting and reading posts. This basically means I can write my posts in
simple markdown, with additional features like the cool syntax highlighting
you can see in this blog post. Some metadata can also be added to the files.
This post, for example, starts with

    ---
    title: A first post
    date: December 2, 2009
    ---
    
    # A first post

Well, that's all for now, folks. Maybe I will elaborate on hakyll again later,
for some reasons I cannot do that yet (I haven't tested it enough, and the code
is not the best you've ever seen). All comments and suggestions are of course
welcome!

Your most humble and obedient servant,
Jasper Van der Jeugt
