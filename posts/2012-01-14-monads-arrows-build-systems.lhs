---
title: Monads and Arrows: modelling a build system
description: An success story of Arrows
tags: haskell
---

This is a recap of an [older blogpost] of me. I decided to rewrite it after I
wanted to link a friend to it, and I saw the blogpost clearly failed to get the
point across in a more-or-less clear way. So, this blogpost is about a situation
in which Monads fall short, and Arrows (and Applicative) prove to be very
powerful. It assumes some basic familiarity with Monads, familiarity with Arrows
is not necessary. This blogpost:

1. Proposes a simplistic build system model
2. Gives an implementation using Monads, and fails
3. Gives a working implementation using Arrow (and Applicative)

[older blogpost]: /posts/2010-03-26-arrows-dependencies.html

> {-# LANGUAGE Arrows #-}
> import Prelude hiding (id, (.))

> import Control.Applicative (Applicative (..), (<$>))
> import Control.Arrow (Arrow (..), returnA, (>>>))
> import Control.Category (Category (..))
> import Control.Monad ((<=<))
> import Control.Monad.State (StateT)
> import System.Directory (doesFileExist, getModificationTime)

This post uses incremental build systems (think [make] or [ant]) as an example.
These systems allow you to specify commands which are only executed if the
destination file is out-of-date. The reason I'm using this example is that it's
highly applicable in [Hakyll], a static site compiler which is one of my side
projects.

[make]: http://www.gnu.org/software/make/
[ant]: http://ant.apache.org/
[Hakyll]: http://jaspervdj.be/hakyll

Let's use a bottom-up approach and first write a simple function to do only
out-of-date builds. The `runBuild` function checks the modification times of the
dependencies and the destination file, and based on that information, calls or
doesn't call the `IO String` workhorse. This is obviously very limited
functionality, but it's just an example.

> runBuild :: FilePath    -- ^ Destination
>          -> [FilePath]  -- ^ Dependencies
>          -> IO String   -- ^ Workhorse
>          -> IO ()       -- ^ May or may not run the workhorse
> runBuild dest deps f = do
>     exists       <- doesFileExist dest
>     depsModified <- mapM getModificationTime deps
>     case (exists, depsModified) of
>         (False, _) -> run
>         (True, []) -> dontRun
>         (True, _)  -> do
>             destModified <- getModificationTime dest
>             if destModified < maximum depsModified then run else dontRun
>   where
>     dontRun = putStrLn $ "Up to date: " ++ dest
>     run = do
>         putStrLn $ "Building " ++ dest
>         writeFile dest =<< f

Let's implement the Unix [paste] command. We first have a pure version:

[paste]: http://en.wikipedia.org/wiki/Paste_%28Unix%29

> paste :: String -> String -> String
> paste x y = unlines $ zipWith (\x' y' -> x' ++ "\t" ++ y') (lines x) (lines y)

And now we can apply our `runBuild` function:

> testBuild :: IO ()
> testBuild = runBuild "test-io.txt" ["foo.txt", "bar.txt"] $ do
>     x <- readFile "foo.txt"
>     y <- readFile "bar.txt"
>     return $ paste x y

This works fine, but the annoyance is that we manually have to specify our
dependencies: this quickly becomes very tedious. Haskell allows for further
abstraction in many ways, so let's have a look.

Monads
======

Let's see if we can capture this behaviour in a Monad. If we declare our Monad
as a simple datatype which holds the dependencies and the actual workhorse. we
get:

> data BuildM a = BuildM [FilePath] (IO a)

Running is easy:

> runBuildM :: FilePath -> BuildM String -> IO ()
> runBuildM dest (BuildM deps f) = runBuild dest deps f

And a `readFile` could be implemented like:

> readFileM :: FilePath -> BuildM String
> readFileM path = BuildM [path] $ readFile path

However, problems arise when we try to pin down the Monad instance for this
datatype.

> instance Monad BuildM where
>     return x              = BuildM []   $ return x
>     (BuildM deps f) >>= g = BuildM deps $ do
>         -- Where do the dependencies of g's result go?
>         BuildM _ y <- g <$> f
>         y

Clearly, this datatype doesn't allow us to get `f`s dependencies in `mx >>= f`.
We can write the following piece of code, but it won't be correct, as it ignores
the `"bar.txt"` dependency.

> testBuildM :: IO ()
> testBuildM = runBuildM "test-m.txt" $ do
>     x <- readFileM "foo.txt"
>     y <- readFileM "bar.txt"
>     return $ paste x y

Other datatypes are possible, e.g. one could also try something like:

> type BuildM' = StateT [FilePath] IO

This kind of definition leads to another problem: the `mx` in `mx >>= f` will
always be executed, even if everything is up-to-date! And this behaviour is
inherently coupled to the use Monads: consider code like this:

> testBuildM' :: IO ()
> testBuildM' = runBuildM "test-m.txt" $ do
>     x <- readFileM "foo.txt"
>     y <- readFileM $ if length x > 200 then "bar.txt" else "qux"
>     return $ paste x y

We *need* to evaluate x in order to determine the dependencies! This is not how
a build system should work: and it makes it clear that Monads are not a good
choice here.

Arrows
======

Two possibilities will work well here: Arrows and Applicative. I'll demonstrate
the Arrow solution first, because it is more generic [^1].

[^1]: More generic in kind: Arrow has a `* -> * -> *` kind, and Applicative has
      a `* -> *` kind. This is important later on, because it means we can
      reuse our Arrow datatype for the Applicative solution.

The datatype looks a lot like the one used for the Monad instance [^2]:

[^2]: The second field is in fact a [Kleisli] arrow, almost a direct translation
      of the IO monad to the Arrow structure.

[Kleisli]: http://en.wikipedia.org/wiki/Arrow_%28computer_science%29#Kleisli_arrows

> data BuildA a b = BuildA [FilePath] (a -> IO b)

Running this build datatype is also straightforward [^3].

[^3]: Note that another option is:
      `runBuildA :: FilePath -> BuildA a String -> a -> IO ()`.

> runBuildA :: FilePath -> BuildA () String -> IO ()
> runBuildA dest (BuildA deps f) = runBuild dest deps $ f ()

Arrows are a generalized version of functions, and can be used in a similar way.
Each Arrow is also a Category, so we first need to declare a Category instance.
In order to make our `BuildA` an Category, need an identity operation, and
function composition.

The `BuildA a a` identity operation is straightforward to implement: it
obviously has no dependencies: it is a build step which does absolutely nothing.
A composition of two build steps takes the sum of dependencies:

> instance Category BuildA where
>     id                        = BuildA [] return
>     BuildA d1 f . BuildA d2 g = BuildA (d1 ++ d2) (f <=< g)

This is not enough to instantiate an Arrow, though. Two more methods need to be
implemented: `arr` and `first`.

`arr` is reasonably simple and allows the user to "lift" a pure function into an
Arrow. For our example, this yields the type signature
`arr :: (a -> b) -> BuildA a b` -- the implementation is straightforward.

In order to allow the programmer to build computations using Arrows, a mechanism
to pass variables through computations is needed. In our example, we have
`first :: BuildA a b -> BuildA (a, c) (b, c)`: it transforms as simple Arrow
into an Arrow which carries an additonal variable through the computation.

> instance Arrow BuildA where
>     arr f                 = BuildA [] (return . f)
>     first (BuildA deps f) = BuildA deps $ \(x, y) -> do
>         x' <- f x
>         return (x', y)

Let's write the Arrow version of `readFileM` which also automatically adds a
dependency:

> readFileA :: FilePath -> BuildA () String
> readFileA path = BuildA [path] $ \() -> readFile path

Using [Arrow notation], we can now implement a (not very pretty) solution which
does bear a lot of resemblance to `testBuildM`, with the difference that this
version actually works with proper dependency management:

[Arrow notation]: http://www.haskell.org/arrows/syntax.html

> testBuildA :: IO ()
> testBuildA = runBuildA "test-a.txt" $ proc () -> do
>     x <- readFileA "foo.txt" -< ()
>     y <- readFileA "bar.txt" -< ()
>     returnA -< paste x y

However, writing ugly code like this obviously isn't the way we want to go.
Arrow-based code can be made a whole lot prettier if you write as much code as
possible as a processing Arrow. For example, we could write an Arrow-based
variant of `paste` which processes a file by pasting another file next to it:

> pasteFileA :: FilePath -> BuildA String String
> pasteFileA path = proc x -> do
>     y <- readFileA path -< ()
>     returnA -< paste x y

With utilities like this, we can write a much prettier `testBuildA` which
clearly demonstrates the processing approach:

> testBuildA' :: IO ()
> testBuildA' = runBuildA "test-a.txt" $
>     readFileA "foo.txt"  >>>
>     pasteFileA "bar.txt"

Epilogue: Applicative functors
==============================

Arrow and Applicative show similar behaviour in many cases. For our example, we
also could've chosen to implement our solution using Applicative instead of
Arrow. I've chosen Arrow for two reasons:

- It is often more natural to model a building process as an Arrow.
- We can actually write an Applicative instance for the *same* datatype, giving
  the user the freedom of choice!

It's a fun challenge to implement this Applicative instance for the `BuildA`
datatype.

<div onclick="$('#applicative').show(1000)">

<strong>Click here to reveal the solution.</strong>

<div></div></div>  <!-- HACKS -->

<div id='applicative' style='display: none'>

> instance Functor (BuildA a) where
>     fmap f (BuildA deps g) = BuildA deps (fmap f . g)
>
> instance Applicative (BuildA a) where
>     pure x                      = BuildA [] $ const $ return x
>     BuildA d1 f <*> BuildA d2 g = BuildA (d1 ++ d2) $ \x -> f x <*> g x
>
> testBuildApp :: IO ()
> testBuildApp = runBuildA "test-app.txt" $
>     paste <$> readFileA "foo.txt" <*> readFileA "bar.txt"

<div></div></div>
