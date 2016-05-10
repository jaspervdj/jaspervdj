> import Control.Monad (forM_)

In Haskell, it is extremely easy for the programmer to add a quick datatype.  It
does not have to take more than a few lines.  This is useful to add auxiliary,
ad-hoc datatypes.

I don't think this is used enough.  Most libraries and code I see use "heavier"
datatypes: canonical and very well-thought-out datatypes, followed by dozens of
instances and related functions.  These are of course great to work with, but it
doesn't have to be a restriction: adding quick datatypes -- without all these
instances and auxiliary functions -- often makes code easier to read.

The key idea is that, in order to make code as simple as possible, you want to
represent your data as simple as possible.  However, the definition of "simple"
is not the same in every context.  Sometimes, looking at your data from another
perspective makes specific tasks easier.  In those cases, introducing
"quick-and-dirty" datatypes often makes code cleaner.

Let's look at a quick example.  Here, we have a definition of a shopping cart in
a fruit store.

> data Fruit = Banana | Apple | Orange
>     deriving (Show, Eq)
>
> type Cart = [(Fruit, Int)]

And we have a few functions to inspect it.

> cartIsHomogeneous :: Cart -> Bool
> cartIsHomogeneous []                = True
> cartIsHomogeneous ((fruit, _) : xs) = all ((== fruit) . fst) xs

> cartTotalItems :: Cart -> Int
> cartTotalItems = sum . map snd

This is very much like code you typically see in Haskell codebases (of course,
with more complex datatypes than this simple example).

The last function we want to add is printing a cart.  The exact way we format it
depends on what is in the cart.

> printCart :: Cart -> IO ()
> printCart cart = case cart of
>     []           -> putStrLn $ "Your cart is empty"
>     [(fruit, 1)] -> putStrLn $ "You are buying one " ++ show fruit
>     _ | cartIsHomogeneous cart && cartTotalItems cart >= 3 -> do
>               putStrLn $
>                   show (cartTotalItems cart) ++
>                   " " ++ show (fst $ head cart) ++ "s" ++
>                   " for you!"
>               putStrLn "BONUS GET!"
>       | otherwise -> do
>           putStrLn $ "Your cart: "
>           forM_ cart $ \(fruit, num) ->
>               putStrLn $ "- " ++ show num ++ " " ++ show fruit

This is not very nice.  The business logic is interspersed with the printing
code.  We could clean it up by adding additional predicates such as
`cartIsBonus`, but having too many of these predicates leads to a certain kind
of [Boolean Blindness].

[Boolean Blindness]: https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/

Instead, it seems much nicer to introduce a temporary type:

> data CartView
>     = EmptyCart
>     | SingleCart  Fruit
>     | BonusCart   Fruit Int
>     | GeneralCart Cart

This allows us to decompose our `printCart` into two clean parts: classifying
the cart, and printing it.

> cartView :: Cart -> CartView
> cartView []           = EmptyCart
> cartView [(fruit, 1)] = SingleCart fruit
> cartView cart
>     | cartIsHomogeneous cart && cartTotalItems cart >= 3 =
>         BonusCart (fst $ head cart) (cartTotalItems cart)
>     | otherwise = GeneralCart cart

Note that we now have a *single* location where we classify the cart.  This is
useful if we need this information in multiple places.  If we chose to solve the
problem by adding additional predicates such has `cartIsBonus` instead, we would
still have to watch out that we check these predicates in the *same order*
everywhere.  Furthermore, if we need to add a case, we can simply add a
constructor to this datatype, and the compiler will tell us where we need to
update our code [^wall].

[^wall]: If you are compiling with `-Wall`, which is what you really, really
    should be doing.

Our `printCart` becomes very simple now:

> printCart2 :: Cart -> IO ()
> printCart2 cart = case cartView cart of
>     EmptyCart          -> putStrLn $ "Your cart is empty"
>     SingleCart fruit   -> putStrLn $ "You are buying one " ++ show fruit
>     BonusCart  fruit n -> do
>         putStrLn $ show n ++ " " ++ show fruit ++ "s for you!"
>         putStrLn "BONUS GET!"
>     GeneralCart items  -> do
>         putStrLn $ "Your cart: "
>         forM_ items $ \(fruit, num) ->
>             putStrLn $ "- " ++ show num ++ " " ++ show fruit

Of course, it goes without saying that ad-hoc datatypes that only used locally
should not be exported from the module -- otherwise you end up with a mess
again.
