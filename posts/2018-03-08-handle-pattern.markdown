---
title: 'Haskell Design Patterns: The Handle Pattern'
description: 'A neat and simple way to build services in Haskell'
tags: 'haskell'
---

# Introduction

I'd like to talk about a design pattern in Haskell that I've been calling _the
Handle pattern_.  This is far from novel -- I've mentioned this
[before][Skillsmatter Video] and the idea is definitely not mine.  As far as I
know, in fact, it has been around since basically forever[^forever].  Since it
is ridiculously close to what we'd call _common sense_[^common-sense], it's
often used without giving it any explicit thought.

[^forever]: Well, `System.IO.Handle` has definitely been around for a while.

[^common-sense]: If you're reading this article and you're thinking: _"What does
this guy keep going on about?  This is all so obvious!"_ -- Well, that's the
point!

I first started more consciously using this pattern when I was working together
with [Simon Meier] at Better (aka [erudify]).  Simon did a writeup about this
pattern [as well][Service Pattern].  But as I was explaining this idea again at
last week's [HaskellerZ] meetup, I figured it was time to do an update of that
article.

The _Handle pattern_ allows you write stateful applications that interact with
external services in Haskell.  It complements pure code (e.g. your business
logic) well, and it is somewhat the result of iteratively applying the question:

- Can we make it simpler?
- Can we make it simpler still?
- And can we still make it simpler?

The result is a powerful and simple pattern that does not even require
Monads[^monad-io] or Monad transformers to be useful.  This makes it extremely
suitable for beginners trying to build their first medium-sized Haskell
application.  And that does not mean it is beginners-only: this technique has
been applied successfully at several Haskell companies as well.

[^monad-io]: It does require `IO`, but we don't require thinking about `IO` as a
`Monad`.  If this sounds weird -- think of lists.  We work with lists all the
time but we just consider them lists of things, we don't constantly call them
"List Monad" or "The Free Monoid" for that matter.

[Service Pattern]: https://www.schoolofhaskell.com/user/meiersi/the-service-pattern
[Skillsmatter Video]: https://skillsmatter.com/skillscasts/10832-how-to-architect-medium-to-large-scale-haskell-applications
[Simon Meier]: https://github.com/meiersi
[Erudify]: /posts/2013-09-29-erudify.html
[HaskellerZ]: https://www.meetup.com/HaskellerZ/

## Table of contents

1.  [Introduction](#introduction)
2.  [Context](#context)
3.  [The module layout](#the-module-layout)
    a.  [A Database Handle](#a-database-handle)
    b.   [Creating a Handle](#creating-a-handle)
    c.  [Destroying a Handle](#destroying-a-handle)
    d.  [Reasonable safety](#reasonable-safety)
    e.  [Summary of the module layout](#summary-of-the-module-layout)
4.  [Handle polymorphism](#handle-polymorphism)
    a.  [A Handle interface](#a-handle-interface)
    b.  [A Handle implementation](#a-handle-implementation)
5.  [Compared to other approaches](#compared-to-other-approaches)

# Context

In Haskell, we try to capture ideas in beautiful, pure and mathematically sound
patterns, for example _Monoids_.  But at other times, we can't do that.  We
might be dealing with some inherently mutable state, or we are simply dealing
with external code which doesn't behave nicely.

In those cases, we need another approach.  What we're going to describe feels
suspiciously similar to Object Oriented Programming:

- Encapsulating and hiding state inside objects
- Providing methods to manipulate this state rather than touching it directly
- Coupling these objects together with methods that modify their state

As you can see, it is not exactly the same as Alan Kay's [original definition]
of OOP[^oop-haskell], but it is far from the horrible incidents that permeate
our field such as UML, abstract factory factories and broken subtyping.

[^oop-haskell]: And indeed, we will touch on a common way of encoding OOP in
Haskell -- creating explicit records of functions -- but we'll also explain why
this isn't always necessary.

[original definition]: http://wiki.c2.com/?AlanKaysDefinitionOfObjectOriented

Before we dig in to the actual code, let's talk about some disclaimers.

Pretty much any sort of Haskell code can be written in this particular way, but
_that doesn't mean that you should_.  This method relies heavily on `IO`.
Whenever you can write things in a pure way, you should attempt to do that and
avoid `IO`.  This pattern is only useful when `IO` is required.

Secondly, there are many alternatives to this approach: complex monad
transformer stacks, interpreters over free monads, uniqueness types, effect
systems...  I don't want to claim that this method is better than the others.
All of these have advantages and disadvantages, so one must always make a
careful trade-off.

# The module layout

For this pattern, we've got a very well-defined module layout.  I believe this
helps with recognition which I think is also one of the reasons we use
typeclasses like _Monoid_.

When I'm looking at the documentation of libraries I haven't used yet, the types
will sometimes look a bit bewildering.  But then I see that there's an `instance
Monoid`.  That's an "Aha!" moment for me.  I _know_ what a Monoid is.  I _know_
how they behave.  This allows me to get up to speed with this library much
faster!

Using a consistent module layout in a project (and even across projects)
provides, I think, very similar benefits to that.  It allows new people on the
team to learn parts of the codebase they are yet unfamiliar with much faster.

## A Database Handle

Anyway, let's look at the concrete module layout we are proposing with this
pattern.  As an example, let's consider a database.  The type in which we are
encapsulating the state is _always_ called `Handle`.  That is because we
[design for qualified import].

[design for qualified import]: https://mail.haskell.org/pipermail/haskell-cafe/2008-June/043986.html

We might have something like:

~~~~~{.haskell}
module MyApp.Database

data Handle = Handle
    { hPool   :: Pool Postgres.Connection
    , hCache  :: IORef (PSQueue Int Text User)
    , hLogger :: Logger.Handle  -- Another handle!
    , …
    }
~~~~~

The internals of the `Handle` typically consist of static fields and other
handles, `MVar`s, `IORef`s, `TVar`s, `Chan`s... With our `Handle` defined, we
are able to define functions using it.  These are usually straightforward
imperative pieces of code and I'll omit them for brevity[^fugacious]:

[^fugacious]: If you want to see a full example, you can refer to [this
repository](https://github.com/jaspervdj/fugacious) that I have been using to
teach practical Haskell.

~~~~~{.haskell}
module MyApp.Database where

data Handle = …

createUser :: Handle -> Text -> IO User
createUser = …

getUserMail :: Handle -> User -> IO [Mail]
getUserMail = …
~~~~~

Some thoughts on this design:

1.  We call our functions `createUser` rather than `databaseCreateUser`.  Again,
    we're working with qualified imports so there's no need for "C-style" names.

2.  **All functions take the `Handle` as the first argument.**  This is very
    important for consistency, but also for [polymorphism](#handle-polymorphism)
    and code style.

    With code style, I mean that the `Handle` is often a syntactically simpler
    expression (e.g. a name) than the argument (which is often a composed
    expression).  Consider:

    ~~~~~{.haskell}
    Database.createUser database $ userName <> "@" <> companyDomain
    ~~~~~

    Versus:

    ~~~~~{.haskell}
    Database.createUser (userName <> "@" <> companyDomain) database
    ~~~~~

3.  Other `Handle`s (e.g. `Logger.Handle`) are stored in a field of our
    `Database.Handle`.  You could also remove it there and instead have it as an
    argument wherever it is needed, for example:

    ~~~~~{.haskell}
    createUser :: Handle -> Logger.Handle -> Text -> IO User
    createUser = …
    ~~~~~

    I usually prefer to put it inside the `Handle` since that reduces the amount
    of arguments required for functions such as `createUser`.  However, if the
    lifetime of a `Logger.Handle` is very short[^short-lifetime], or if you
    want to reduce the amount of dependencies for `new`, then you could consider
    doing the above.

4.  The datatypes such as `Mail` may be defined in this module may even be
    specific to this function.  I've written about [ad-hoc datatypes] before.

[^short-lifetime]: With a short lifetime I mean you would create a new
`Logger.Handle` for every call to `createUser`.  But even in that case you
could consider turning `Logger.Handle` into something like a resource pool,
from which you could request a new concrete logging interface to log things.
It really depends on your use case in the end...

[ad-hoc datatypes]: /posts/2016-05-11-ad-hoc-datatypes.html

## Creating a Handle

I mentioned before that an important advantage of using these patterns is that
programmers become "familiar" with it.  That is also the goal we have in mind
when designing our API for the creation of `Handle`s.

In addition to always having a type called `Handle`, we'll require the module to
always have a type called `Config`.  This is where we encode our static
configuration parameters -- and by static I mean that we shouldn't have any
`IORef`s or the like here: this `Config` should be easy to create from pure
code.

~~~~~{.haskell}
module MyApp.Database where

data Config = Config
    { cPath :: FilePath
    , …
    }

data Handle = …
~~~~~

We can also offer some way to create a `Config`.  This really depends on your
application.  If you use the [configurator] library, you might have something
like:

~~~~~{.haskell}
parseConfig :: Configurator.Config -> IO Config
parseConfig = …
~~~~~

On the other hand, if you use [aeson] or [yaml], you could write:

~~~~~{.haskell}
instance Aeson.FromJSON Config where
    parseJSON = …
~~~~~

You could even use a [Monoid][Partial Options Monoid] to support loading
configurations from multiple places.  But I digress -- the important part is
that there is a type called `Config`.

Next is a similar pattern: in addition to always having a `Config`, we'll also
always provide a function called `new`.  The parameters follow a similarly
strict pattern:

~~~~~{.haskell}
new :: Config         -- 1. Config
    -> Logger.Handle  -- 2. Dependencies
    -> …              --    (usually other handles)
    -> IO Handle      -- 3. Result
~~~~~

Inside the `new` function, we can create some more `IORef`s, file handles,
caches... if required and then store them in the `Handle`.

[configurator]: https://hackage.haskell.org/package/configurator
[aeson]: https://hackage.haskell.org/package/aeson
[yaml]: https://hackage.haskell.org/package/yaml
[Partial Options Monoid]: https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67

## Destroying a Handle

We've talked about creation of a `Handle`, and we mentioned the normal functions
operating on a `Handle` (e.g. `createUser`) before.  So now let's consider the
final stage in the lifetime of `Handle`.

Haskell is a garbage collected language and we can let the runtime system take
care of destroying things for us -- but that's not always a great idea.  Many
resources (file handles in particular come to mind as an example) are scarce.

There is quite a strong correlation between scarce resources and things you
would naturally use a `Handle` for.  That's why I recommend always providing a
`close` as well, even if does nothing.  This is a form of forward compatibility
in our API: if we later decide to add some sort of log files (which needs a
`close`), we can do so without individually mailing all our module users that
they now need to add a `close` to their code.

~~~~~{.haskell}
close :: Handle -> IO ()
close = …
~~~~~

## Reasonable safety

When you're given a `new` and `close`, it's often tempting to add an auxiliary
function like:

~~~~~{.haskell}
withHandle
    :: Config            -- 1. Config
    -> Logger.Handle     -- 2. Dependencies
    -> …                 --    (usually other handles)
    -> (Handle -> IO a)  -- 3. Function to apply
    -> IO a              -- 4. Result, handle is closed automatically
~~~~~

I think this is a great idea.  In fact, it's sometimes useful to _only_ provide
the `withHandle` function, and hide `new` and `close` in an internal module.

The only caveat is that the naive implementation of this function:

~~~~~{.haskell}
withHandle config dep1 dep2 … depN f = do
    h <- new config dep1 dep2 … depN
    x <- f h
    close h
    return x
~~~~~

Is **wrong**!  In any sort of `withXyz` function, you should always use
`bracket` to guard against exceptions.  This means the correct implementation
is:

~~~~~{.haskell}
withHandle config dep1 dep2 … depN f =
    bracket (new config dep1 dep2 … depN) close f
~~~~~

Well, it's even shorter!  In case you want more information on why `bracket` is
necessary, [this blogpost][Well Typed Exceptions] gives a good in-depth
overview.  My summary of it as it relates to this article would be:

1. Always use `bracket` to match `new` and `close`
2. You can now use `throwIO` and `killThread` safely

It's important to note that `withXyz` functions do not provide complete safety
against things like use-after-close our double-close.  There are many
interesting approaches to fix these issues but they are _way_ beyond the scope
of this tutorial -- things like [Monadic Regions] and [The Linearity Monad] come
to mind.  For now, we'll rely on `bracket` to catch common issues and on code
reviews to catch team members who are not using `bracket`.

[Well Typed Exceptions]: http://www.well-typed.com/blog/97/
[Monadic Regions]: http://okmij.org/ftp/Haskell/regions.html
[The Linearity Monad]: https://www.cis.upenn.edu/~jpaykin/papers/pz_linearity_monad_2017.pdf

## Summary of the module layout

If we quickly summarise the module layout, we now have:

~~~~~{.haskell}
module MyApp.Database
    ( Config (..)   -- Internals exported
    , parseConfig   -- Or some other way to load a config

    , Handle        -- Internals usually not exported
    , new
    , close
    , withHandle

    , createUser  -- Actual functions on the handle
    , …
    ) where
~~~~~

This is a well-structured, straightforward and easy to learn organisation.  Most
of the `Handle`s in any application should probably look this way.  In the next
section, we'll see how we can build on top of this to create dynamic,
customizable `Handle`s.

# Handle polymorphism

It's often important to split between the interface and implementation of a
service.  There are countless ways to do this in programming languages.  For
Haskell, there is:

- Higher order functions
- Type classes and type families
- Dictionary passing
- Backpack module system
- Interpreters over concrete ASTs
- ...

The list is endless.  And because Haskell on one hand makes it so easy to
abstract over things, and on the other hand makes it possible to abstract over
pretty much anything, I'll start this section with a disclaimer.

_Premature_ abstraction is a real concern in Haskell (and many other high-level
programming languages).  It's easy to quickly whiteboard an abstraction or
interface and unintentionally end up with completely the wrong thing.

It usually goes like this:

1. You need to implement a bunch of things that look similar
2. You write down a typeclass or another interface-capturing abstraction
3. You start writing the actual implementations
4. One of them doesn't _quite_ match the interface so you need to change it two
   weeks in
5. You add another parameter, or another method, mostly for one specific
   interface
6. This causes some problems or inconsistencies for interfaces
7. Go back to (4)

What you end up with is a leaky abstraction that is the _product_ of all
concrete implementations -- where what you really wanted is the _greatest common
divisor_.

There's no magic bullet to avoid broken abstractions so my advice is usually to
first painstakingly do all the different implementations (or at least a few of
them).  _After_ you have something working and you have emerged victorous from
horrible battles with the guts of these implementations, _then_ you could start
looking at what the different implementations have in common.  At this point,
you'll also be a bit wiser about where they differ -- and you'll be able to take
these important details into account, at which point you retire from just being
an idiot drawing squares and arrows on a whiteboard.

This is why I recommend sticking with simple `Handle`s until [you really need
it].  But naturally, sometimes we really need the extra power.

[you really need it]: https://en.wikipedia.org/wiki/You_aren%27t_gonna_need_it

## A Handle interface

So let's do the simplest thing that can possibly work.  Consider the following
definition of the `Handle` we discussed before:

~~~~~{.haskell}
module MyApp.Database
    ( Handle (..)  -- We now need to export this
    ) where

data Handle = Handle
    { createUser :: Text -> IO User
    , …
    }
~~~~~

What's the type of `createUser` now?

~~~~~{.haskell}
createUser :: Handle -> Text -> IO User
~~~~~

It's exactly the same as before!  This is pretty much a requirement: it means we
can move our `Handle`s to this approach when we need it, not when we envision
that we will need it at some point in the future.

## A Handle implementation

We can now create a concrete implementation for this abstract `Handle` type.
We'll do this in a module like `MyApp.Database.Postgres`.

~~~~~{.haskell}
module MyApp.Database.Postgres where
import MyApp.Database

data Config = …

new :: Config -> Logger.Handle -> … -> IO Handle
~~~~~

The `Config` datatype and the `new` function have now moved to the
implementation module, rather than the interface module.

Since we can have any number of implementation modules, it is worth mentioning
that we will have multiple `Config` types and `new` functions (exactly one of
each per implementation).  Configurations are always specific to the concrete
implementation.  For example, an [sqlite] database may just have a `FilePath` in
the configuration, but our `Postgres` implementation will have other details
such as port, database, username and password.

[sqlite]: https://sqlite.org/index.html

In the implementation of `new`, we simply initialize a `Handle`:

~~~~~{.haskell}
new config dep1 dep2 … depN = do
    -- Intialization of things inside the handle
    …

    -- Construct record
    return Handle
        { createUser = \name -> do
            …
        , …
        }
~~~~~

Of course, we can manually float out the body of `createUser` since constructing
these large records gets kind of ugly.

# Compared to other approaches

We've presented an approach to modularize the effectful layer of medium- to
large-scaled Haskell applications.  There are many other approaches to tackling
this, so any comparison I come up with would probably be inexhaustive.

Perhaps the most important advantage of using `Handle`s is that they are first
class values that we can freely mix and match.  This often does not come for
free when using more exotic strategies.

Consider the following type signature from a Hackage package -- and I do not
mean to discredit the author, the package works fine but simply uses a different
approach than my personal preference:

~~~~~{.haskell}
-- | Create JSON-RPC session around conduits from transport layer.
-- When context exits session disappears.
runJsonRpcT
    :: (MonadLoggerIO m, MonadBaseControl IO m)
    => Ver                  -- ^ JSON-RPC version
    -> Bool                 -- ^ Ignore incoming requests/notifs
    -> Sink ByteString m () -- ^ Sink to send messages
    -> Source m ByteString  -- ^ Source to receive messages from
    -> JsonRpcT m a         -- ^ JSON-RPC action
    -> m a                  -- ^ Output of action
~~~~~

I'm a fairly experienced Haskeller and it still takes me a bit of eye-squinting
to see how this will fit into my application, especially if I want to use this
package with other libraries that do not use the `Sink`/`Source` or
`MonadBaseControl` abstractions.

It is somewhat obvious that one running call to `runJsonRpcT` corresponds to
being connected to one JSON-RPC endpoint, since it takes a single sink and
source.  But what if we want our application to be connected to multiple
endpoints at the same time?

What if we need to have hundreds of thousands of these, and we want to store
them in some priority queue and only consider the most recent ones in the
general case.  How would you go about that?

You could consider running a lightweight thread for every `runJsonRpcT`, but
that means you now need to worry about thread overhead, communicating exceptions
between threads and killing the threads after you remove them.  Whereas with
first-class handles, we would just have a `HashPSQ Text Int JsonRpc.Handle`,
which is much easier to reason about.

- - -

So, I guess one of the oldest and most widely used approaches is MTL-style monad
transformers.  This uses a hierarchy of typeclasses to represent access to
various subsystems.

I love working with MTL-style transformers in the case of pure code, since they
often allow us to express complex ideas concisely.  For effectful code, on the
other hand, they do not seem to offer many advantages and often make it harder
to reason about code.

My personal preference for writing complex effectful code is to reify the
effectful operations as a datatype and then write pure code manipulating these
effectful operations.  An interpreter can then simply use the `Handle`s to
perform the effects.  For simpler effectful code, we can just use `Handle`s
directly.

I have implemented a number of these patterns in the (ever unfinished) example
web application [fugacious](https://github.com/jaspervdj/fugacious), in case you
want to see them in action or if you want a more elaborate example than the
short snippets in this blogpost.  Finally, I would like to thank [Alex
Lang](https://github.com/alang9/) and [Nicolas Mattia](http://www.nmattia.com/)
for proofreading, and [Titouan Vervack](https://github.com/tivervac) for many
corrections and typos.
