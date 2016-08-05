> {-# LANGUAGE BangPatterns #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE DeriveTraversable #-}

> import qualified Data.Text as T
> import qualified Data.HashSet as HS
> import Data.String (IsString (..))
> import qualified Data.Vector as V
> import qualified Data.HashMap.Strict as HMS
> import qualified Data.Foldable as F

In Haskell, it's very common to model structured documents as tree-like
structures.  The ubiquitous Aeson library is maybe the most well-known example,
but many applications and libraries end up defining their own because they need
to deal with a few special cases.

You end up with something like this:

> data Value1
>     = Array1 !(V.Vector Value1)
>     | Object1 !(HMS.HashMap T.Text Value1)
>     | String1 !T.Text
>     | Int1 !Int
>     deriving (Eq, Show)

> -- Just for readability
> instance IsString Value1 where fromString = String1 . T.pack

> test01 :: Value1
> test01 = Object1 $ HMS.fromList
>     [ ("name",    "tommy")
>     , ("friends", Array1 $ V.fromList ["timmy", "tammy"])
>     , ("age",     Int1 14)
>     ]

Many operations on these recursive data structures are recursive itself.  One
example here is creating a `HS.HashSet` of all the strings that appear in a
document.  We are using a strict accumulator to get reasonably efficient code.

> valStringSet1 :: Value1 -> HS.HashSet T.Text
> valStringSet1 = go HS.empty
>   where
>     go !acc (Array1  v)   = V.foldl' go acc v
>     -- go !acc (Object1 obj) = HMS.foldl' go acc obj
>     go !acc (Object1 _)   = acc  -- Oops!
>     go !acc (String1 t)   = HS.insert t acc
>     go !acc (Int1    _)   = acc

However, as the document datatype gets more complex, it is very easy to _forget
a recursive case_, and you end up traversing only a part of it.

Creating a big `foldValue1` function which takes a higher-order-function
argument for each constructor is one solution, but it is not extremely pretty.
Sometimes you only need to act on one constructor and then you are still
required to pass in functions for the other arguments.

A more elegant solution is defining the recursive datatype as a `Functor`.  This
requires very little code, as GHC can derive the `Functor`, `Foldable` and
`Traversable` instances for us!

> data FValue2 a
>     = Array2 !(V.Vector a)
>     | Object2 !(HMS.HashMap T.Text a)
>     | String2 !T.Text
>     | Int2 !Int
>     deriving (Eq, Foldable, Functor, Show, Traversable)

As you can see, we took out the recursive appearances by replacing them with the
argument of the functor `a`.

Then, we can "tie the knot" by instantiating the functor with itself.

> newtype Value2 = Value2 {unValue2 :: FValue2 Value2}
>     deriving (Eq, Show)

> -- Just for readability
> instance IsString Value2 where fromString = Value2 . String2 . T.pack

It just became very easy to write recursive functions against `Value2`: we can
use all the standard functions such as `fmap`, `fold` and `traverse`.  This is
the rewritten `valStringSet1` which uses `foldl'` from [Data.Foldable].

> valStringSet2 :: Value2 -> HS.HashSet T.Text
> valStringSet2 = F.foldl' go HS.empty . unValue2
>   where
>     go !acc (Value2 (String2 t)) = HS.insert t acc
>     go !acc (Value2 v)           = F.foldl' go acc v  -- Catch them all

> test02 :: Value2
> test02 = Value2 $ Object2 $ HMS.fromList
>     [ ("name",    "tommy")
>     , ("friends", Value2 $ Array2 $ V.fromList ["timmy", "tammy"])
>     , ("age",     Value2 (Int2 14))
>     ]
