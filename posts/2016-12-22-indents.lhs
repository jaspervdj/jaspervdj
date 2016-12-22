---
title: 'Package takeover: indents'
description: A Parsec package to deal with indentation
tags: haskell
---

Parsers are one of [Haskell]'s indisputable strengths.  The most well-known
library is probably [Parsec]. This parser combinator library has been around
since at least 2001, but is still widely used today, and it has inspired [new
generations] of general purpose parsing libraries.

[Haskell]: https://www.haskell.org/
[Parsec]: https://hackage.haskell.org/package/parsec
[new generations]: https://hackage.haskell.org/package/megaparsec

Parsec makes it really easy to prototype parsers for certain classes of
grammars.  Lots of grammars in use today, however, are whitespace-sensitive.
There are different approaches for dealing with that.  One of the most commonly
used approaches is to add explicit `INDENT` and `DEDENT` tokens.  But that
usually requires you to add a separate lexing phase -- not a bad idea by itself,
but a bit annoying if you are just writing a quick prototype.

![`^(\t{2,})(\S.*)\n(?:\1\t.*\n)*` can get you only so far in life](/images/2016-12-22-bart.jpg)

That is why I like the [indents] package -- it sits in a sweet spot because it
is a straightforward package that allows you turn any Parsec parser into an
indentation-based one without having to change too many types.

[indents]: https://hackage.haskell.org/package/indents

It offers a bunch of semi-cryptic operators like `<+/>` and `<*/>` which I would
personally avoid in favor of their named variants, but other than that I would
consider it a fairly "easy" package.

Unfortunately, I found a few bugs an inconveniences in the old package.  One
interesting bug would allow failing branches of the parse to still affect the
indentation's internal state, which is very bad [^state-bug].  Additionally, the
package fixed the underlying monad, which prevented you from using transformers.

[^state-bug]: See <http://lpaste.net/344393>.

Because I didn't want to confuse people by creating yet another package, I [took
over] the package which is a very smooth process nowadays.  I can definitely
recommend this to anyone who discovers issues like these in unmaintained
packages.  The hackage trustees are doing great and valuable work there.

[took over]: https://mail.haskell.org/pipermail/haskell-cafe/2016-November/125582.html

I have now uploaded a new version which fixes these issues.  To celebrate that,
let's create a toy parser for indentation-sensitive taxonomies such as the big
tea taxonomy [^tea-source]:

    tea
      green
        korean
          pucho-cha
          chung-cha
        vietnamese
          snow-green-tea
        japanese
          roasted
            ...
      black
        georgian
          traditional
          caravan-blend
        african
          kenyan
          tanzanian
        ...

[^tea-source]: The interesting tea taxonomy can be found in this blogpost:
    <https://jameskennedymonash.wordpress.com/mind-maps/amazing-tea-taxonomy/>.

We need some imports to get rolling.  After all, this blogpost is a [literate
haskell file] which can be loaded in `GHCi`.

[literate haskell file]: https://raw.githubusercontent.com/jaspervdj/jaspervdj/master/posts/2016-12-22-indents.lhs

> import           Control.Applicative ((*>), (<*), (<|>))
> import qualified Text.Parsec         as Parsec
> import qualified Text.Parsec.Indent  as Indent

We just store a single term in the category as a `String`.

> type Term = String

A taxonomy is then recursively defined as a `Term` and its children taxonomies.

> data Taxonomy = Taxonomy Term [Taxonomy] deriving (Eq, Show)

A parser for a term is easy.  We just parse an identifier and then skip the
spaces following that.

> pTerm :: Indent.IndentParser String () String
> pTerm =
>     Parsec.many1 allowedChar <* Parsec.spaces
>   where
>     allowedChar = Parsec.alphaNum <|> Parsec.oneOf ".-"

In the parser for a `Taxonomy`, we use the `indents` library.  `withPos` is used
to "remember" the indentation position.  After doing that, we can use
combinators such as `indented` to check if we are indented past that point.

> pTaxonomy :: Indent.IndentParser String () Taxonomy
> pTaxonomy = Indent.withPos $ do
>     term <- pTerm
>     subs <- Parsec.many $ Indent.indented *> pTaxonomy
>     return $ Taxonomy term subs

Now we have a simple main to function to put it all together;

> readTaxonomy :: FilePath -> IO Taxonomy
> readTaxonomy filePath = do
>     txt <- readFile filePath
>     let errOrTax = Indent.runIndentParser parser () filePath txt
>     case errOrTax of
>         Left  err -> fail (show err)
>         Right tax -> return tax
>   where
>     parser = pTaxonomy <* Parsec.eof

And we can verify that this works in GHCi:

    *Main> readTaxonomy "taxonomy.txt"
    Taxonomy "tea" [Taxonomy "green" [Taxonomy "korean" [...
    *Main>

Special thanks to Sam Anklesaria for writing the original package.
