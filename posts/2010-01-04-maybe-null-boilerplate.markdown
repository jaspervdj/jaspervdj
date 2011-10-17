---
title: Maybe, null, and boilerplate code
description: An ode to the Maybe type in Haskell, and a comparison with null
tags: haskell
---

A Haskell novice is usually quickly introduced to the `Maybe` type. I think
there are many good tutorials out there covering the `Maybe` type, so I won't
be explaining it here -- instead, I will make a comparison between `Maybe` and
`null` pointers. I also briefly use monads in the end -- I will not explain them
here either, there already are a lot of tutorials about them (hint: google for
"burritos"[^1]).

[^1]: Sorry, inside joke here.

I have chosen to compare `null` pointers from Java -- I could have chosen `nil`
from Ruby, or `None` from Python -- this does not really matter here.

## Why null?

Why are `null` pointers present in almost every object-oriented language? A
first goal they serve is as "unassigned variables". I will not cover that here,
instead I will focus on another use: abnormal return values.

From the Java HashMap documentation:

>     public Object get(Object key)
>
> Returns the value to which the specified key is mapped in this identity
> hash map, or null if the map contains no mapping for this key.

So, `null` is some sort of "Not found" error here. But wait -- they could
have used an Exception here!

## Why not exceptions?

Well, I see a few reasons why didn't use exceptions here:

- Exceptions are much slower for various reasons.
- They have to be caught again, which leads to very verbose code.
- Nobody really likes exceptions.
- Compatability with the old `Hashtable` class?

I realize I'm not being very complete here -- if you think you know the real
reason, feel free to comment. Anyway, if we look at the type signature, we
see a similar behavior in the `Map` type in Haskell.

~~~~~{.haskell}
lookup :: (Ord k) => k -> Map k a -> Maybe a
~~~~~

We see a similar behavior here: when the object is found, we get a `Just a`,
and when it is not found, we get `Nothing` -- comparable to a `null` pointer.

## The danger of null

`null` is very cool and all, but I think this cartoon from
[Geek and Poke](http://geekandpoke.typepad.com/) illustrates its danger:

![NullPointerException cartoon](/images/2010-01-04-null-pointer-exception.png)

So, while we can get a `null` *back* from a function, we should never give it
*to* a function.

## A simple scenario

On to a simple scenario, based on a problem I recently encountered. Given a
`Map`, we want to check for a `"path"` key, which contains a filename in the
form of `yyyy-mm-dd-title.extension`. We have two functions here:

~~~~~{.haskell}
lookup :: (Ord k) => k -> Map k a -> Maybe a
parseDate :: String -> Maybe Date
~~~~~

I'm using an imaginary, simplified `parseDate` function here for illustration
purposes. The idea is that it returns `Nothing` when the given string is
malformed. Suppose we have corresponding funtions in Java:

~~~~~{.java}
public Object get(Object key)
public Date parseDate(String str)
~~~~~

Here, the second value returns `null` if the argument is malformed. Of course -
the `parseDate` function crashes if we call `parseDate(null)` (as is usual in
Java), so we have to be careful as always here. We have the following snippet
in Haskell:

~~~~~{.haskell}
getPathMonth :: Map String String -> Maybe Month
getPathMonth m = 
    case M.lookup "path" m of
        Nothing  -> Nothing
        (Just p) -> case parseDate p of
            Nothing  -> Nothing
            (Just d) -> Just (getMonth d)
~~~~~

Which corresponds to the following Java code:

~~~~~{.java}
public Month getPathMonth(Map<String, String> m)
{
    p = m.get("path");
    if(p == null) return null;
    d = parseDate(p);
    if(d == null) return null;
    return d.getMonth();
}
~~~~~

As you can see, the two fragments are very similar.

## The superiority of Maybe

Now, if you have written a lot of Java code, you know that checking for `null`
is a vital part of the job, and fragments like the one above are pretty common.

In Haskell, however, `Maybe` is also a *monad* -- and monads can be used to
prevent common patterns in code -- in other words, they assist you in the
[D.R.Y.](http://en.wikipedia.org/wiki/Don't_repeat_yourself)-principle[^2]. We
can therefore write the above snippet again using `do`-notation[^3].

[^2]: Monads do more than this, of course. But, as I said, there are many good
      [tutorials](http://www.haskell.org/all_about_monads/html/index.html) out
      there covering them.

[^3]: copumpkin from the #haskell channel pointed out that you could also write
      it as an epic oneliner:

          getPathMonth = return . getMonth <=< parseDate <=< M.lookup "path"

~~~~~{.haskell}
getPathMonth :: Map String String -> Maybe Month
getPathMonth m = do p <- lookup "path" m
                    d <- parseDate p
                    return $ getMonth d
~~~~~

Now, the whole "check-for-null-and-short-circuit" behavior is defined by the
`Maybe` monad -- and so, we don't have to repeat ourselves. I have not yet found
a similar way of preventing repeated patterns like this in Java. If you have any
ideas, feel free to inform me[^4].

[^4]: I'm not boasting about Haskell here, I'm genuinely interested in ways to
      do this in Java.

Also, some kudos to the people in [#zeus irc](http://zeus.ugent.be) for some
things on exceptions.

Your most humble and obedient servant,
Jasper Van der Jeugt
