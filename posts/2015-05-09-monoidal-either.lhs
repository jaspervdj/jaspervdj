---
title: Can we write a Monoidal Either?
description: About the problems with clashing Applicative and Monad
tags: haskell
---

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> import           Control.Applicative (Applicative (..))
> import           Data.Monoid         (Monoid, (<>))

<!-- This is needed for compilation but irrelevant to the blogpost so we'll take
it out. -->
<div style="display: none">

> newtype Expr = Expr String
> instance Show Expr where show (Expr x) = x
> type Type = String
> type TypeScope = [Type]
> type Scope = String
> type Program = String

> expr1, expr2 :: Expr
> expr1 = Expr "False == 3"
> expr2 = Expr "[1, 'a']"

> program1 :: Program
> program1 = undefined

</div>

Introduction
============

For the last month or, I have been working as a contractor for [Luminal]. I am
helping them implement [Fugue], and more specifically Ludwig -- a compiler for
statically typed declarative configuration language. This is one of the most
interesting projects I have worked on so far -- writing a compiler is really
fun. While implementing some parts of this compiler, I came across an
interesting [discussion].

[Luminal]: http://luminal.io/
[Fugue]: https://www.fugue.it/
[discussion]: https://twitter.com/jaspervdj/status/596308326671081473

The problem
===========

Suppose we have a typical Either-like type: some code can succeed (`Ok`), or an
error might occur (`Failed`).

> data Check e a
>     = Failed e
>     | Ok     a
>     deriving (Eq, Show)

We can write a straightforward Functor instance:

> instance Functor (Check e) where
>     fmap _ (Failed e) = Failed e
>     fmap f (Ok     x) = Ok (f x)

The Monad instance is also very obvious:

> instance Monad (Check e) where
>     return x = Ok x
>
>     Failed e >>= _ = Failed e
>     Ok     x >>= f = f x

However, the Applicative instance is not that obvious -- we seem to have a
choice.

But first, lets take a step back to formulate the problem a bit more clearly and
give some more context. Imagine we have the following types in our compiler:

~~~~~{.haskell}
data Type = ...
data Expr = ...
data Program = ...
type TypeScope = [Type]
~~~~~

And our code looks like this:

> findDefinedTypes1 :: Program -> Check String TypeScope
> findDefinedTypes1 _ = Ok []  -- Assume we can't define types for now.

> typeCheck1 :: TypeScope -> Expr -> Check String Type
> typeCheck1 _ e = Failed $ "Could not typecheck: " ++ show e

> compiler1 :: Check String ()
> compiler1 = do
>     scope <- findDefinedTypes1 program1
>     typeCheck1 scope expr1  -- False == 3
>     typeCheck1 scope expr2  -- [1, 'a']
>     return ()

On executing `compiler1`, we get the following error:

    *Main> compiler1
    Failed "Could not typecheck: False == 3"

Which is correct, but using a compiler entirely written in this fashion would be
annoying. `Check`, like `Either`, short-circuits on the first error it
encounters. This means we would compile our program, fix one error, compile, fix
the next error, compile, and so on.

It would be much nicer if users were to see multiple error messages at once.

Of course, this is not always possible. On one hand, if `findDefinedTypes1`
throws an error, we cannot possibly call `typeCheck1`, since we do not have a
`TypeScope`.

On the other hand, if `findDefinedTypes1` succeeds, shouldn't it be possible to
collect error messages from both `typeCheck1 scope expr1` *and* `typeCheck1
scope expr2`?

It turns out this is possible, precisely because the second call to `typeCheck1`
does not depend on the result of the first call -- so we can execute them in
parallel, if you will. And that is precisely the difference in expressive power
between Monad and Applicative: Monadic `>>=` needs access to previously computed
results, where Applicative `<*>` does not. Let's *(ab?)*use this to our
advantage.

The solution?
=============

Cleverly, we put together following instance:

> instance Monoid e => Applicative (Check e) where
>     pure x = Ok x
>
>     Ok     f  <*> Ok     x  = Ok (f x)
>     Ok     _  <*> Failed e  = Failed e
>     Failed e  <*> Ok     _  = Failed e
>     Failed e1 <*> Failed e2 = Failed (e1 <> e2)

Using this instance we can effectively *collect* error messages. We need to
change our code a bit to support a *collection* of error messages, so let's use
`[String]` instead of `String` since a list is a Monoid.

> findDefinedTypes2 :: Program -> Check [String] TypeScope
> findDefinedTypes2 _ = Ok []  -- Assume we can't define types for now.

> typeCheck2 :: TypeScope -> Expr -> Check [String] Type
> typeCheck2 _ e = Failed ["Could not typecheck: " ++ show e]

> compiler2 :: Check [String] ()
> compiler2 = do
>     scope <- findDefinedTypes2 program1
>     typeCheck2 scope expr1 *> typeCheck2 scope expr2
>     return ()

Note that `*>` is the Applicative equivalent of the Monadic `>>`.

Now, every error is represented by a *list* of error messages (typically a
singleton such as in `typeCheck2`), and the Applicative `<*>` combines error
messages. If we execute `compiler2`, we get:

    *Main> compiler2
    Failed ["Could not typecheck: False == 3",
            "Could not typecheck: [1, 'a']"]

Success! But is that all there is to it?

The problem with the solution
=============================

The problem is that we have created a situation where `<*>` is not equal to
`ap` [^ap]. After researching this for a while, it seems that `<*> = ap` is not
a verbatim rule. However, most arguments suggest it should be the case -- even
the name.

[^ap]: `ap` is the Monadic sibling of `<*>` (which explains why `<*>` is
commonly pronounced `ap`). It can be implemented on top of `>>=`/`return`:

    ~~~~~{.haskell}
    > ap :: Monad m => m (a -> b) -> m a -> m b
    > ap mf mx = do
    >     f <- mf
    >     x <- mx
    >     return (f x)
    ~~~~~

This is important for refactoring, for example. Quite a few Haskell programmers
(including myself) would refactor:

~~~~~{.haskell}
do b <- bar
   q <- qux
   return (Foo b q)
~~~~~

Into:

~~~~~.haskell
Foo <$> bar <*> qux
~~~~~

Without putting too much thought in it, just assuming it does the same thing.

In our case, they are clearly *similar*, but not *equal* -- we would get only
one error instead of collecting error messages. One could argue that this is
*close enough*, but when one uses that argument too frequently, you might just
end up with something like PHP.

The problem becomes more clear in the following fragment:

~~~~~{.haskell}
checkForCyclicImports modules >>
compileAll modules
~~~~~

Which has completely different behaviour from this fragment:

~~~~~{.haskell}
checkForCyclicImports modules *>
compileAll modules
~~~~~

The latter will get stuck in some sort of infinite recursion, while the former
will not. This is not a subtle difference anymore. While the problem is easy to
spot here (`>>` vs. `*>`), this is not always the case:

~~~~~{.haskell}
forEveryImport_ :: Monad m => Module -> (Import -> m ()) -> m ()
~~~~~

Ever since [AMP], it is impossible to tell whether this will do a `forM_` or a
`for_`-like traversal without looking at the implementation -- this makes
making mistakes easy.

The solution to the problem with the solution
=============================================

As we discussed in the previous section, it should be possible for a programmer
to tell exactly how a Monad or Applicative will behave, without having to dig
into implementations. Having a structure where `<*>` and `ap` behave slightly
differently makes this hard.


When a Haskell programmer wants to make a clear distinction between two similar
types, the first thing that comes to mind is probably `newtype`s. This problem
is no different.

Let's introduce a newtype for error-collecting Applicative.  Since the Functor
instance is exactly the same, we might as well generate it using
`GeneralizedNewtypeDeriving`.

> newtype MonoidCheck e a = MonoidCheck {unMonoidCheck :: Check e a}
>     deriving (Functor, Show)

Now, we provide our Applicative instance for `MonoidCheck`:

> instance Monoid e => Applicative (MonoidCheck e) where
>     pure x = MonoidCheck (Ok x)
>
>     MonoidCheck l <*> MonoidCheck r = MonoidCheck $ case (l, r) of
>         (Ok     f , Ok     x ) -> Ok (f x)
>         (Ok     _ , Failed e ) -> Failed e
>         (Failed e , Ok     _ ) -> Failed e
>         (Failed e1, Failed e2) -> Failed (e1 <> e2)

Finally, we *avoid* writing a Monad instance for `MonoidCheck`. This approach
makes the code cleaner:

- This ensures that when people use `MonoidCheck`, they are forced to use the
  Applicative combinators, and they cannot *accidentally* reduce the number of
  error messages.

- For other programmers reading the code, it is very clear whether we are
  dealing with short-circuiting behaviour or that we are collecting multiple
  error messages: it is explicit in the types.

Usage and conversion
====================

Our fragment now becomes:

> findDefinedTypes3 :: Program -> Check [String] TypeScope
> findDefinedTypes3 _ = Ok []  -- Assume we can't define types for now.

> typeCheck3 :: TypeScope -> Expr -> MonoidCheck [String] Type
> typeCheck3 _ e = MonoidCheck $ Failed ["Could not typecheck: " ++ show e]

> compiler3 :: Check [String] ()
> compiler3 = do
>     scope <- findDefinedTypes3 program1
>     unMonoidCheck $ typeCheck3 scope expr1 *> typeCheck3 scope expr2
>     return ()

We can see that while it is not more *concise*, it is definitely more *clear*:
we can see exactly which functions will collect error messages. Furthermore, if
we now try to write:

~~~~~{.haskell}
typeCheck3 scope expr1 >> typeCheck3 scope expr2
~~~~~

We will get a compiler warning telling us we should use `*>` instead.

Explicitly, we now convert between `Check` and `MonoidCheck` by simply calling
`MonoidCheck` and `unMonoidCheck`. We can do this inside other transformers if
necessary, when your stack is more complex than just `Either`:

~~~~~{.haskell}
mapReaderT MonoidCheck
    :: ReaderT r (Check e) a -> ReaderT r (MonoidCheck e) a
mapReaderT unMonoidCheck
    :: ReaderT r (MonoidCheck e) a -> ReaderT r (Check e) a
~~~~~

Conclusion
==========

The `MonoidCheck` discussed in this blogpost is available as
[Data.Either.Validation] on hackage. The main difference is that instead of
using a `newtype`, the package authors provide a full-blown datatype.

[Data.Either.Validation]: https://hackage.haskell.org/package/either-4.3.3.2/docs/Data-Either-Validation.html

> data Validation e a
>     = Failure e
>     | Success a

And two straightforward conversion functions:

> validationToEither :: Validation e a -> Either e a
> validationToEither (Failure e) = Left e
> validationToEither (Success x) = Right x

> eitherToValidation :: Either e a -> Validation e a
> eitherToValidation (Left e)  = Failure e
> eitherToValidation (Right x) = Success x

This makes constructing values a bit easier:

~~~~~{.haskell}
Failure ["Can't go mucking with a 'void*'"]
~~~~~

Instead of:

~~~~~{.haskell}
MonoidCheck $ Failed ["Can't go mucking with a 'void*'"]
~~~~~
