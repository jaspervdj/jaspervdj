---
title: Erasing "expected" messages in Parsec
description: A useful little trick to improve parser errors
tags: haskell
---

Introduction
============

[Parsec] is an industrial-strength parser library. I think one of its main
advantages is that allows you generate really good error messages. However, this
sometimes requires some non-obvious tricks. In this blogpost, I describe one of
those. On the way, we illustrate how one can split up a Parsec parser into a
lexer and an actual parser.

This blogpost assumes a little familiarity with Parsec or parser combinator
libraries. There are tons of Parsec tutorials out there, such as [this
one](https://kunigami.wordpress.com/2014/01/21/an-introduction-to-the-parsec-library/).

[Parsec]: https://hackage.haskell.org/package/parsec

**TL:DR:** Using `<?> ""` allows you to erase error messages which in some cases
can actually improve them.

This blogpost is written in literate Haskell so you should be able to just load
it up in GHCi and play around with it (you can find the raw `.lhs` file
[here](https://github.com/jaspervdj/jaspervdj/raw/master/posts/2015-09-02-erasing-parsec-expected-messages.lhs)).

A simple expression parser
==========================

> {-# LANGUAGE FlexibleContexts #-}
> import Control.Monad (void)
> import Text.Parsec

As an example, let's build a simple [Polish notation] parser to parse
expressions like:

[Polish notation]: http://en.wikipedia.org/wiki/Polish_notation

    + 2 (+ 1 4)

We can model the expressions we want to parse like this:

> data Expr
>     = Lit Int
>     | Add Expr Expr
>     deriving (Show)

Our parser is pretty straightforward -- there are three cases: literals,
additions, and expressions enclosed by parentheses.

> expr :: Stream s m Char => ParsecT s u m Expr
> expr = (<?> "expression") $
>     (Lit <$> natural)               <|>
>     (plus >> Add <$> expr <*> expr) <|>
>     (lparen *> expr <* rparen)

This uses the auxiliary parsers `natural`, `plus`, `lparen` and `rparen`. These are
so-called *token* parsers. It is a common design pattern to split up a parser
into a lexer (in this case, we call the collection of token parsers the lexer)
and the actual parser [^split].

[^split]: Traditionally, the lexer and parser are actually split into separate
phases, where the lexer produces a `Token` datatype stream from the input
`String`. Parsec, however, also allows you to write both at the same time, which
is what we do in this blogpost. Both approaches have advantages and
disadvantages.

The idea behind that is that the lexer takes care of fiddling with whitespace,
comments, and produces tokens -- atomic symbols of the language such as `123`,
`+`, `(`, and `)`. The parser can then focus on the actual logic: parsing
expressions. It doesn't need to care about details such as whitespace.

Lexer
=====

Now, onto the token parsers. These are typically placed in another module.
First, let's build some tools for dealing with whitespace and comments. Parsec
already provides a parser to consume white space (`spaces`), so let's add one
for a comment:

> -- | Consume a comment (from a '#' character to the end of the line) and
> -- return nothing.
> comment :: Stream s m Char => ParsecT s u m ()
> comment = (<?> "comment") $ char '#' >> void (manyTill anyChar endOfLine)

Using `comment` and `spaces`, we can build a parser that skips both:

> whitespace :: Stream s m Char => ParsecT s u m ()
> whitespace = do
>     spaces
>     optional $ comment >> whitespace

Now, let's define a token parser a bit more clearly: a token parser is a parser
which consumes an atomic symbol followed by an arbitrary amount of `whitespace`.

This way, we can just use token parsers after one another in the parser and it
is clear that the whitespace in between two tokens is consumed by the first
token parser. Then, we only have to remember to strip whitespace from the
beginning of the file when we write the top-level parser, like:

    whitespace *> expr

Before we define our token parsers, let's add a quick combinator which
facilitates it:

> lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
> lexeme p = p <* whitespace

We can use `lexeme` to define some simple tokens:

> plus, lparen, rparen :: Stream s m Char => ParsecT s u m Char
> plus   = lexeme $ char '+'
> lparen = lexeme $ char '('
> rparen = lexeme $ char ')'

Followed by a slightly more complicated token:

> -- | Parse one or more digits as a decimal integer
> natural :: Stream s m Char => ParsecT s u m Int
> natural = (<?> "number") $ lexeme $ do
>     x  <- try digit
>     xs <- many digit
>     return $ read (x : xs)

That's it! Now we have our parser. If we parse the following expression:

    + (+ 1 2)
      (+ 3
         # Four is a really cool number
         4)

We get:

    Add (Add (Lit 1) (Lit 2)) (Add (Lit 3) (Lit 4))

Looking good!

Erasing "expected" error messages
=================================

At last, we arrive at the point of this blogpost. Let's try to parse the
following expression:

    + (+ 1 2)
      (- 2 3)

We get the following error:

    unexpected "-"
    expecting white space, comment or expression

The error message is correct but a bit verbose. Sure, there could be a comment
or whitespace at that position, but the user is probably aware of that. The real
issue is that the parser is expecting an expression.

In the Parsec documentation, there is no reference to how one can manipulate
this message. However, when we take a closer look at the Parsec source code, it
turns out that there is a way: using `<?>` with the empty string `""`.

I think treating the empty string as a special case is a bit un-Haskelly --
`<?>` would be more self-documenting if it took a `Maybe String` as its second
argument -- but it is what it is.

`<?> ""` is a bit confusing to read -- it is not immediately clear what it does
so let's turn it into a named combinator for clarity:

> eraseExpected :: ParsecT s u m a -> ParsecT s u m a
> eraseExpected = (<?> "")

We can rewrite `whitespace` using this combinator.

> whitespace' :: Stream s m Char => ParsecT s u m ()
> whitespace' = do
>     skipMany $ eraseExpected space
>     optional $ eraseExpected comment >> whitespace'


Notice that we had to inline the definition of spaces before erasing the error
message. This is because `<?>` only sets the error message if the parser fails
*without* consuming any input. This means that:

    eraseExpected spaces

Would not erase the error message if at least one space character is consumed.
Hence, we use `skipMany $ eraseExpected space`.

If we fix `lexeme` to use the new `whitespace'`, we get a much nicer error
message (in the spirit of *less is more*):

    unexpected "-"
    expecting expression

Thanks to Alex Sayers for proofreading.
