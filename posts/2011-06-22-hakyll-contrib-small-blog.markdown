---
title: Create a small blog using hakyll-contrib
description: Create your blog software in 15 minutes with Hakyll
tags: hakyll, code
---

## What?

One of my side projects is [Hakyll], a static site generator written in Haskell.
Altough the name is a play on [Jekyll], Hakyll bears more resemblance to
[nanoc].

[Hakyll]: http://jaspervdj.be/hakyll
[Jekyll]: http://jekyllrb.com
[nanoc]: http://nanoc.stoneship.org

The resemblance is there because nanoc's author [Denis] is a good friend of mine,
who also lives close to me, and we've had a number of interesting discussions on
static site generators.

[Denis]: http://stoneship.org

Both nanoc and Hakyll offer a lot of flexibility, behaving more like a web
publishing API/library than a simple tool. This brings me to (in my opinion) the
key feature of Jekyll: it is *simple*. You can get started with a website by not
much more than simply throwing a bunch of text files in a directory and running
the tool.

Today, I've released a package [hakyll-contrib] which allows just that. The rest
of this blogpost is a tutorial, which is also included in the Haddock
documentation.

[hakyll-contrib]: http://hackage.haskell.org/package/hakyll-contrib

## Installation

The idea is that you don't have to write your configuration yourself: you
just follow some conventions, and Hakyll does the rest. Start by installing the
tool:

    cabal install hakyll-contrib

You can generate a site which will serve as a good starting point by running
the command-line tool:

    hakyll-contrib small-blog

`small-blog` is the template used -- I might add more templates later. Hakyll
will generate a simple example site for you. The necessary configuration is
placed in the `small-blog.hs` file. Compile and run it to create the demo site:

    ghc --make small-blog.hs
    ./small-blog build
    ./small-blog preview

Then, visit [the site](http://localhost:8000) in your browser.

# Further conventions

Images should be placed in the `images/` or `img/` folder. The are copied
directly. Other static files (but not images) can be placed in `static/` or
`files/`. The `favicon.ico` file is an exception, it is just placed in the
top-level directory.

CSS files should be placed in `css/`, and JavaScript files in `js/`.

Then, we arrive at pages. You can create any number of pages on your site: just
create files in one of the documents pandoc supports (`.html`, `.markdown`,
`.rst`, `.lhs`...) in the top-level directory.

These pages may use a number of preconfigured `$key$`'s:

* `$recentPosts$`: A list of recent posts, displayed from most recent to
  oldest. By default, 3 posts are shown, altough this can be configured using
  the 'numberOfRecentPosts' field.

* `$allPosts$`: A list of all posts, displayed from most recent to oldest.
  This is very useful for creating an archive page.

* `$chronologicalPosts$`: Similar to `$allPosts$`, but displays the posts in
  chronological order.

For example usage, look at the example site we generated using
`hakyll-contrib small-blog`.

Now, one can wonder where these posts come from. Simple: all pages in the
`posts/` directory are considered posts. Note that a naming format of

    posts/year-month-date-title.extension

is mandatory. An example:

    posts/2011-06-19-hello-world.markdown

This allows Hakyll to parse the date easily, among other things. Again, look
at the example site for some example posts.

Additionaly, there is the `templates/` folder. This folder holds the
templates for your site. For a `small-blog` configuration, your site should
have *exactly* three templates:

* `templates/default.html`: The main template. This should contain your
  HTML doctype, head, etc.

* `templates/post.html`: A template which is applied to every post before
  it is rendered using the default template.

* `templates/post-item.html`: A template which is applied to posts in
  listings (e.g. `$chronologicalPosts$`).

Again, the example should clarify things.

This configuration should be enough to create a small personal website. But,
we have only touched the surface of what is possible with Hakyll. For more
information, check out the [tutorials](http://jaspervdj.be/hakyll).
