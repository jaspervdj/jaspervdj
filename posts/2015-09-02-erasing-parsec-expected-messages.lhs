---
title: Erasing "expected" messages in Parsec
description: A useful little trick to improve parser errors
tags: haskell
---

Introduction
============

[Parsec] is an industrial-strength parser library. I think one of its main
advantages is that allows you generate really good error messages. However, this
sometimes requires some non-obvious tricks. In this blogpost, I describe on of
those. On the way, we illustrate how one splits up a Parsec parser into a lexer
and an actual parser.

[Parsec]: https://hackage.haskell.org/package/parsec

**TL:DR:** Using `<?> ""` allows you to erase error messages which can actually
improve them.

A simple expression parser
==========================

> {-# LANGUAGE FlexibleContexts #-}
> import Control.Monad (void)
> import Text.Parsec

As an example, lets build a simple [Polish notation] parser to parse expressions
like:

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
>     (Lit <$> int)                   <|>
>     (plus >> Add <$> expr <*> expr) <|>
>     (lparen *> expr <* rparen)

This uses the auxiliary parsers `int`, `plus`, `lparen` and `rparen`. These are
so-called *token* parsers. It is a common design pattern to split up a parser
into a lexer (in this case, we call the collection of token parsers the lexer)
and the actual parser.

The idea behind that is that the lexer take care of fiddling with whitespace,
comments, and produces tokens -- atomic symbols of the language such as `123`,
`+`, `(`, and `)`. The parser can then focus on the actual logic: parsing
expressions. It doesn't need to know about details such as whitespace.

Lexer
=====

Now, onto the token parsers. These are typically placed in another module.
First, let's build some tools for dealing with whitespace and comments. Parsec
already provides a parser to consume white space (`spaces`), so let's add one
for a comment:

> comment :: Stream s m Char => ParsecT s u m ()
> comment = (<?> "comment") $ char '#' >> void (manyTill anyChar endOfLine)

Using `comment` and `spaces`, we can build a parser that skips both:

> whiteSpace :: Stream s m Char => ParsecT s u m ()
> whiteSpace = do
>     spaces
>     optional $ comment >> whiteSpace

Now, let's define a token parser a bit more clearly: a token parser is a parser
which consumes an atomic symbol followed by an arbitrary amount of `whiteSpace`.

This way, we can just use token parsers after one another in the parser and it
is clear that the whitespace in between two tokens is consumed by the first
token parser. For whitespace in the beginning of the file, we then "close" the
parser by using something like:

    whiteSpace *> expr

Before we define our token parsers, let's add a quick combinator chich
facilitates it:

> lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
> lexeme p = p <* whiteSpace

We can use `lexeme` to define some simple tokens:

> plus, lparen, rparen :: Stream s m Char => ParsecT s u m Char
> plus   = lexeme $ char '+'
> lparen = lexeme $ char '('
> rparen = lexeme $ char ')'

Followed by a slightly more complicated token:

> int :: Stream s m Char => ParsecT s u m Int
> int = (<?> "number") $ lexeme $ do
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

In the Pandoc documentation, there is no reference to how one can manipulate
this message. However, there is a way: using `<?>` with the empty string `""`.
This seems a bit un-Haskelly and I guess it would be more self-documenting `<?>`
if would take a `Maybe String`, but it is what it is.

`<?> ""` is a bit confusing to read -- it is not immediately clear what it does
-- so let's add a combinator for clarity:

> eraseUnexpected :: ParsecT s u m a -> ParsecT s u m a
> eraseUnexpected = (<?> "")

We can rewrite `whiteSpace` using this combinator.

> whiteSpace' :: Stream s m Char => ParsecT s u m ()
> whiteSpace' = do
>     skipMany $ eraseUnexpected space
>     optional $ eraseUnexpected comment >> whiteSpace'

We needed to be a bit more careful when we replace `spaces`: `<?>` only sets the
error message if the parser fails *without* consuming any input. This means
that:

    eraseUnexpected spaces

Would not erase the error message if at least one space character is consumed.
Hence, we use:

    skipMany $ eraseUnexpected space

If we fix `lexeme` to use the new `whitespace'`, we get a much nicer error
message (less is more):

    unexpected "-"
    expecting expression
