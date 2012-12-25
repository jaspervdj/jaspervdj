---
title: Applicative, bidirectional serialization combinators
description: A neat GADT/Applicative trick
tags: haskell
---

Unrelated
=========

*If you are only interested in the neat Haskell trick, skip this section.* It
has been a while since I updated my personal blog here. There has been a lot
going on in my life in the past few months, including exams, a breakup with my
girlfriend and moving to Tokyo for an internship at [Tsuru Capital].

[Tsuru Capital]: http://www.tsurucapital.com/

It has been really great so far, the work is interesting, so are the people, and
we have a nice view from the office.

![View from the Tsuru Capital office](/images/2012-09-07-view-from-tsuru.jpg)

Prologue
========

This blogpost is written in literate Haskell ([source here]), so you can drop it
in a file, load it in `GHCi` and play around with it, should you feel like doing
this.

[source here]: https://github.com/jaspervdj/jaspervdj/blob/master/posts/2012-09-07-applicative-bidirectional-serialization-combinators.lhs

> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> import Control.Applicative
> import Control.Monad
> import Data.Aeson

A lot of serialization libraries use a technique employing two typeclasses (or
one typeclass with two methods) in order to convert data from and to some
format. An example is the excellent [aeson] library:

[aeson]: http://hackage.haskell.org/package/aeson

> data Food = Food
>     { foodName :: String
>     , foodCost :: Int
>     } deriving (Show)

> instance ToJSON Food where
>     toJSON (Food name cost) = object
>         [ "name" .= name
>         , "cost" .= cost
>         ]

> instance FromJSON Food where
>     parseJSON (Object obj) = Food
>         <$> obj .: "name"
>         <*> obj .: "cost"
>     parseJSON _ = mzero

Some other libraries using this technique are [cassava], [postgresql-simple],
and [binary].

[cassava]: http://hackage.haskell.org/package/cassava
[postgresql-simple]: http://hackage.haskell.org/package/postgresql-simple
[binary]: http://hackage.haskell.org/package/binary

While working on some new internal [SQLite] bindings and utilities at Tsuru, I
discovered another technique which is more concise, but keeps the nice
`Applicative` interface. The code in this blogpost disregards performance and
only supports the most basic of features in order to be easier to understand.

[SQLite]: http://www.sqlite.org/

Primitive types
===============

We start out by defining a typeclass for primitive serializable types, such as
`Int` and `String`. We disregard performance, as said before, and we are also
going to ignore proper error checking: we just serialize these primitive types
to `String`s.

> class Field a where
>     fieldType :: a -> String  -- Should work with 'undefined'
>     fieldRead :: String -> a
>     fieldShow :: a -> String

> instance Field String where
>     fieldType = const "TEXT"
>     fieldRead = id  -- We need proper escaping here
>     fieldShow = id

> instance Field Int where
>     fieldType = const "INTEGER"
>     fieldRead = read
>     fieldShow = show

These fields are sufficient to store some `Food`, so what else do we need? We
need "instantiations" of these fields which actually represent a part of a
record. This can be modeled as:

> data FieldInfo t a = FieldInfo
>     { fieldName    :: String
>     , fieldExtract :: t -> a
>     }

Let's give an example, we would map the cost of food as:

> costFieldInfo :: FieldInfo Food Int
> costFieldInfo = FieldInfo "cost" foodCost

*This `costFieldInfo` is not used in the code below, it is just an example.*

GADTs
=====

In the case of an SQLite database, such a `FieldInfo` actually represent a
column in our table. We will be building our table using `Applicative`,
*without* actually evaluating anything yet. This seems related to *free monads*
-- determining the exact category-theoretical relation is left as an excercise
for the mathematical-minded reader.

We just have constructors for each method of the applicative interface, plus an
SQLite primitive to add a column to our table.

This requires the great [GADTs] extension, it is not possible to do this with
Haskell 98 datatypes -- the wiki page has more information.

[GADTs]: http://www.haskell.org/haskellwiki/GADT

> data Table t f where
>     -- Applicative interface:
>     -- <$>, pure and <*>
>     Map  :: (a -> b) -> Table t a -> Table t b
>     Pure :: a -> Table t a
>     App  :: Table t (a -> b) -> Table t a -> Table t b
>
>     -- Primitives
>     Column :: Field a => FieldInfo t a -> Table t a

This makes the `Functor` and `Applicative` instances trivial to implement.

> instance Functor (Table t) where
>     fmap = Map

> instance Applicative (Table t) where
>     pure  = Pure
>     (<*>) = App

This GADT allows us to model actual tables:

> foodTable :: Table Food Food
> foodTable = Food
>     <$> Column (FieldInfo "name" foodName)
>     <*> Column (FieldInfo "cost" foodCost)

Let's make some nicer syntax and write `foodTable` again:

> column :: Field a => String -> (t -> a) -> Table t a
> column name extract = Column (FieldInfo name extract)

> foodTable' :: Table Food Food
> foodTable' = Food
>     <$> column "name" foodName
>     <*> column "cost" foodCost

Lookin' good!

The real work
=============

While procrastrinating by creating GADT wrappers is fun, at one point we should
write some actual implementation.

These implementations work by evaluating the trees we created with the different
constructors for the GADT.

Our first method crawls the table tree and returns the name and type of each
column:

> metaRecord :: Table t t -> [(String, String)]
> metaRecord = go
>   where
>     go :: forall t a. Table t a -> [(String, String)]
>     go (Map _ t)   = go t
>     go (Pure _ )   = []
>     go (App t1 t2) = go t1 ++ go t2
>     go (Column fi) = [(fieldName fi, fieldType (undefined :: a))]

The second method is only slightly more complicated: instead of returning the
type of each column, it calls our custom extract function for that column to get
its value. We can then call `fieldShow` on that to do the final serialization,
and what we get is the serialized name and value of each column.

> toRecord :: forall t. Table t t -> t -> [(String, String)]
> toRecord tab x = go tab
>   where
>     go :: forall a. Table t a -> [(String, String)]
>     go (Map _ t)   = go t
>     go (Pure _ )   = []
>     go (App t1 t2) = go t1 ++ go t2
>     go (Column fi) =
>         [(fieldName fi, fieldShow $ fieldExtract fi x)]

The deserialization method is the hardest. It works by actually *evaluating* the
tree we built. Because of our GADT use, we can do this in a type-safe way.

> fromRecord :: forall t. Table t t -> [(String, String)] -> t
> fromRecord tab record = go tab
>   where
>     go :: forall a. Table t a -> a
>     go (Map f t)   = f (go t)
>     go (Pure x)    = x
>     go (App ft t)  = (go ft) (go t)
>     go (Column fi) = case lookup (fieldName fi) record of
>         Nothing  -> error $ "Missing field: " ++ fieldName fi
>         Just str -> fieldRead str

Utilities
=========

We add a small utility function to pretty-print our records:

> printRecord :: [(String, String)] -> IO ()
> printRecord = putStrLn . unlines . map (\(x, y) -> x ++ " = " ++ y)

And a typeclass so we do not have to create an `xxxTable` function for each
datatype:

> class HasTable t where
>     table :: Table t t

Demo
====

Now let's implement food serialization for real: we only need to implement a
*single* typeclass with a *single* method!

> instance HasTable Food where
>     table = Food
>         <$> column "name" foodName
>         <*> column "cost" foodCost

Some concrete deliciousness:

> ramen :: Food
> ramen = Food "ラーメン" 800

And a trivial demo:

> main :: IO ()
> main = do
>     putStrLn "Meta record (used in CREATE TABLE... etc.):"
>     printRecord $ metaRecord (table :: Table Food Food)
>
>     putStrLn "Serialized ramen:"
>     printRecord (toRecord table ramen)
>
>     putStrLn "Deserialized sashimi:"
>     print (fromRecord table [("name", "刺身"), ("cost", "1200")] :: Food)

Conclusion
==========

I think this is a nice use case of GADTs, and although it has a performance
impact, it looks like it is worth it (for now). If it turns out that we can
nicely generalize this code, we will probably release it on Hackage. But feel
free to steal the idea, and comments are also welcome, of course!

Exercise for the reader: can you make `Table` a `Monad`? Why (not)?
