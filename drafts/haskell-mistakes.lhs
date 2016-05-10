Introduction
============

This talk is about "beginner" mistakes people make when using Haskell. It is not
about trivial mistakes -- syntax or type errors: it is about mistakes that are
able to slip through the safety net that Haskell's sophisticated type system
provides.

Pick your battles
=================

With enough time and resources, it's probably possible to have perfectly Haskell
clean code everywhere. Of course, very few projects have these resources
available!

So it is important to decide where you put your time. A few options:

1. Do you spend a lot of timing formatting code so that it looks aesthetically
   pleasing?
2. Do you want to focus on getting the job done above everything else, and you
   don't mind building a bit of technical debt?
3. Do you mostly care about performance and are you willing to do some nasty
   things (TM) to get fast code?
4. Should all top-level functions have proper haddock, documentation and tests?

Usually I try to focus on the module level:

- The *exposed interface* of a module should be clean (inside you can
  `unsafePerformIO` all you want).
- The module should have some documentation about what it is for.
- There should be some tests that check that the module work correctly (but you
  don't need to test *all* the internals).

Not compiling with `-Wall`
==========================

By default, only a subset of useful warnings is turned on. This does not
include:

- `-fwarn-unused-binds`               (medium-risk)
- `-fwarn-unused-matches`             (high-risk)
- `-fwarn-unused-imports`             (low-risk)
- `-fwarn-incomplete-patterns`        (high-risk)
- `-fwarn-dodgy-exports`              (low-risk)
- `-fwarn-dodgy-imports`              (low-risk)
- `-fwarn-incomplete-uni-patterns`    (high-risk)
- `-fwarn-incomplete-record-updates`  (high-risk)
- `-fwarn-monomorphism-restriction`   (low-risk)
- `-fwarn-auto-orphans`               (medium-risk)
- `-fwarn-implicit-prelude`           (low-risk)
- `-fwarn-missing-local-sigs`         (low-risk)
- `-fwarn-missing-exported-sigs`      (low-risk)
- `-fwarn-missing-import-lists`       (low-risk)
- `-fwarn-identities`                 (low-risk)

Note that the high-risk ones can cause runtime crashes.

Wildcard pattern matches
========================

Wildcard pattern matches are useful in that they allow you to express something
very succinctly. Suppose we have the following datatype:

> data IsMurderer
>     = Innocent
>     | Murderer
>     deriving (Eq)

> canBeReleased :: IsMurderer -> Bool
> canBeReleased Innocent = True
> canBeReleased Murderer = False

> shouldBeExecuted :: IsMurderer -> Bool
> shouldBeExecuted Innocent = False
> shouldBeExecuted _        = True

> fireSquad :: IsMurderer -> Bool
> fireSquad x = x /= Innocent

What happens if we change the datatype:

> data IsMurderer2
>     = Innocent2
>     | Murderer2
>     | Like99PercentSure

Testing your code
=================

In the Haskell ecosystem, there is an abundancy of QuickCheck/Smallcheck
tutorials and libraries. These allow you to easily a bunch of pure functions, or
a pure core of your project.

However, less has been written about testing your entire application, which is
at least as important.

A few packages are available which focus on this (`hspec-snap`,
`hspec-webdrivers`, `tasty-golden`...). Often it is not enough if you need to
set up complex environments and in those cases resorting to another language
(Bash, Python, JavaScript...) is better than not having any tests.

Using ByteString for text
=========================

There seems to be a lot of confusion about what libraries should be used to
represent `String`s because there are different datatypes. However, the
situation is not that complex:

- Use `String` if you're dealing with short, human-readable text, and you're not
  really worried about performance.
- `Text` is an "optimization" of `String`, better suited if you have a lot of
  human-readable text and you want to have lower memory usage and faster
  processing.
- `ByteString` should really be called `Bytes` and is not suited for
  human-readable text, only for binary data.

    Are we talking about binary data or human-readable text?
    -> binary data: use `ByteString`
    -> human-readable text:
        Are you writing hello world?
        -> Yes: Use `String`
        -> No: Use `Text`

Secondly, there is the question of whether or not you should use the lazy
variants of `Text`/`ByteString`.

    Will bad stuff happen if you load all your data into the memory?
    -> Yes: Use the strict version
    -> No:
        Are you making a single pass over the data?
        -> Yes:
            Are you really aware of all the problems associated with lazy I/O?
            -> Yes: Use the lazy variants.
            -> No: Use something like Pipes/Conduit/io-stream
        -> No:
            mmap?

Thinking you are understand exceptions
======================================

Reasoning about laziness + exceptions can be hard.

    main = do
        -- What if we put bang patterns here?
        let x01 = throwIO $ ErrorCall "throwIO"
            x02 = throw   $ ErrorCall "throw"
        x01
        x02

Reasoning about laziness + asynchronous exceptions can be absolute madness.

Does this function always close the socket?

    foo :: (Socket -> IO a) -> IO a
    foo f = do
        s <- openSocket
        r <- try $ f s
        closeSocket s
        ...

How about?

    foo :: (Socket -> IO a) -> IO a
    foo f = mask $ \restore -> do
        s <- openSocket
        r <- try $ restore $ f s
        closeSocket s
        ...

Typical train of thought when dealing with exceptions:

1. It's easy, this function should now throw any exceptions.
2. The exception is coming from another function, let's use `mask`.
3. Maybe we want to timeout so perhaps we should use `unsafeUnmask` here?
4. ???
5. Nothing works and programming is an eternal cycle of pain and darkness.
6. Go back to 1.

Since reasoning about exceptions is so hard, you should try to avoid most of the
madness by:

1. Keeping your code pure where possible (obviously).
2. Try to avoid throwing exceptions from pure code. If you have an exception
   lurking somewhere deep within a thunk, you (usually) have a problem.
3. Use a more predictable `IO (Either e a)` where you have to.
4. Use existing solutions such as `resource-pool`, `resourcet`, and
   really focus on understanding operations like `bracket`.

Avoiding GHC Extensions
=======================

Initially I was very wary of using GHC extensions. At that point I came more or
less from a C/C++ background and I had experienced lots of pain trying to get
things to compile on MSVC++, so naturally I wanted to avoid these
"Locks-Me-In-To-GHC" extensions.

- There is only GHC.
- Extensions are widely used throughout base packages.
- It's Haskell so refactoring is relatively easy.

Latest production code base: average of around 3 extensions per module (modules
are usually very small).

Most common extensions:

- `OverloadedStrings`
- `TemplateHaskell`
- `DeriveDataTypeable`
- `ScopedTypeVariables`
- `GeneralizedNewtypeDeriving`
- `BangPatterns`

Extensions that improve scoping/syntax (and hence readability) are almost always
benevolent. E.g.:

- `LambdaCase`
- `ViewPatterns`
- `PatternGuards`
- `MultiWayIf`
- `TupleSections`
- `BinaryLiterals`

Being afraid of `unsafePerformIO`
=================================

- Is it really bad if the IO is performed twice?
- 
