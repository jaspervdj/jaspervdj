---
title: 'Text/UTF-8: Aftermath'
description: The final results, and what's next?
tags: haskell
---

# What is this?

The [Google Summer of Code][gsoc] deadline ends about now, so it is time to
reflect on the progress I've made. Yesterday, we had a conference call with
[Bryan O'Sullivan], [Johan Tibell] and my mentor [Edward Kmett] in order to
discuss what we should do with my results and patches.

[gsoc]: http://socghop.appspot.com
[Bryan O'Sullivan]: http://www.serpentine.com/blog/
[Johan Tibell]: http://blog.johantibell.com/
[Edward Kmett]: http://comonad.com/

# TL;DR

The patches will *not* be merged into the trunk for now. However, we do consider
this project succesful. Read on...

# The general idea

By converting from UTF-16 to UTF-8, I slowed down a lot of functions. This is
inevitable, because most functions in the `Data.Text` library are implemented as
something like:

~~~~~{.haskell}
f :: Text -> Text
f = unstream . f' . stream

g :: Text -> Text
g = unstream . g' . stream
~~~~~

In this case, `f'` is a function that works on *characters* instead of bytes,
and `stream`/`unstream` do the conversion from and to the byte array.
`Data.Text` has a stream fusion framework, which means that a function such as

~~~~~{.haskell}
h :: Text -> Text
h = f . g
~~~~~

is (by GHC) translated to

~~~~~{.haskell}
h :: Text -> Text
h = unstream . f' . stream . unstream . g' . stream
~~~~~

which, in turn, gets optimized using stream fusion:

~~~~~{.haskell}
h :: Text -> Text
h = unstream . f' . g' . stream
~~~~~

What does this mean for us? We know that functions such as `f'` and `g'` remain
exactly the same when using either UTF-16 or UTF-8: only the `stream` and
`unstream` functions are affected. And since they now deal with UTF-8, which is
a little more complicated, functions such as `h` are generally *more expensive*.

So where is the benefit from porting the library to UTF-8? This comes from the
fact that most applications use something like:

~~~~~{.haskell}
p :: ByteString -> ByteString
p = encodeUtf8 . h . decodeUtf8
~~~~~

and when the internal encoding is UTF-8 as well, we can optimize both
`decodeUtf8` and `encodeUtf8`.

# The results

This was an experimental project from the start, since we did not actually know
whether or not these changes would make the text library faster -- which is why
benchmarking was such an important part of it.

It turns out that, as we predicted, it *usually* is an optimization, albeit a
very small one, and it is not certain if the relatively small speedup will be
worth all of the work involved in completely switching to UTF-8. While I
completely ported the text library, it would still involve:

- lot of testing, since we can't break one of the "core libraries"
- porting of `text-icu`, which is far from trivial

The list of external libraries which uses UTF-16 (and with which we might wish
to communicate) is also fairly large: ICU, Java, .Net, Cocoa...

This might (or might not) outweigh the fact that we would usually have better
(less) memory usage for UTF-8.

So, because there was not really a very compelling reason for Bryan to choose
for either of the two, he chose to keep the UTF-16 version for now, because this
[requires less work](http://en.wikipedia.org/wiki/Path_of_least_resistance).

# More about memory usage

One of the reasons to switch to UTF-8 was the face that memory usage could be
reduced for ASCII text, which is very common in markup languages. However, a
`Text` value takes 6 words, plus the payload in bytes. This means that
converting to UTF-8 usually would not be such a big gain in terms of memory
usage if you have lots of small `Text` values!

- On a 32 bit system, you need a string of at least 24 characters before the
  payload outweighs the overhead.
- On a 64 bit system, this becomes a string of at least 48 characters.

That being said, UTF-8 still has some advantages here: for example, when
checking two `Text` values for equality, the less bytes you need to check, the
better.

# Was all my work in vain?

Certainly not! What future is there for all the patches I've written?

- Many of them have already been merged into the trunk: we now have a better
  benchmark suite for future optimizations and the tests have also gotten a
  big cleanup.
- A lot of optimizations I've applied can also be applied to the UTF-16 version.
  This is very interesting, because it means I will be able to make text faster
  in the near future by just porting patches.
- The idea is to maintain the UTF-8 fork for now, as future GHC versions might
  be able to fix some issues we have with branch prediction. It is not
  unthinkable that we will switch once there is a compelling reason to do so.

So, as a finishing note to conclude this project (or at least the official
timespan of it, because I am looking forward to doing some more work with text
and my patches), thanks again to my mentor(s) Bryan, Edward and Johan!
