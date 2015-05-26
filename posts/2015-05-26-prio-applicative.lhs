---
title: The Prio Applicative
description: An interesting Applicative which lets us prioritize computations
tags: haskell
---

Introduction
============

When writing some code recently, I came across a very interesting Applicative
Functor. I wanted to write about for two reasons:

- It really shows the power of Applicative (compared to Monad). Applicative does
  not require access to previously computed results, which helps in this case,
  because it allows us to execute statements in whatever order is convenient.

- I think it is novel, I was digging for a bit and could not find a similar
  Applicative in any Haskell code.

This blogpost is written in literate Haskell so you should be able to just load
it up in GHCi and play around with it (you can find the raw `.lhs` file
[here](https://github.com/jaspervdj/jaspervdj/raw/master/posts/2015-05-26-prio-applicative.lhs)).

> {-# LANGUAGE BangPatterns        #-}
> {-# LANGUAGE GADTs               #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> import           Control.Applicative (Applicative (..), (<$>))
> import           Control.Monad       (forM, liftM, liftM2)
> import           Control.Monad.State (State, runState, state)
> import           Data.List           (sortBy)
> import           Data.Ord            (comparing)
> import           Data.Traversable    (traverse)
> import qualified Data.Vector         as V

The problem
===========

In our example, we will be modeling a dessert restaurant.

> type Dessert = String

We keep the inventory of our restaurant simply as a list. The important
invariant here is that the inventory is always ordered from cheapest to most
expensive.

> type Inventory = [Dessert]

> defaultInventory :: Inventory
> defaultInventory =
>     [ "Pancake"
>     , "Apple Pie"
>     , "Apple Pie"
>     , "Tiramisu"
>     ]

Whenever a client wants to order something, they have two options:

- Request a specific dessert;
- Just get the cheapest one we have available.

In the first case, they will not get served anything if the specific dessert is
out of stock. In the second case, they will only miss out on a dessert when our
inventory is completely empty.

> data Request
>     = RequestSpecificDessert Dessert
>     | RequestCheapestDessert
>     deriving (Show)

Let's implement the logic for serving a request. We use `State Inventory` to
keep track of what's available.

> doRequest :: Request -> State Inventory (Maybe Dessert)

For `RequestCheapestDessert`, we make use of the fact that our inventory is
sorted by price. This means the head of the list is the cheapest dessert, so we
serve that and put the tail of the list (`xs`) back.

> doRequest RequestCheapestDessert =
>     state $ \inventory -> case inventory of
>         []             -> (Nothing, [])
>         (dessert : xs) -> (Just dessert, xs)

In case the client wants a specific dessert, we use `break` to take out the
requested item from the inventory list.

> doRequest (RequestSpecificDessert requested) =
>     state $ \inventory -> case break (== requested) inventory of
>         (xs, dessert : ys) -> (Just dessert, xs ++ ys)
>         (xs, [])           -> (Nothing, xs)


<div style="display: none">

> test01 = runState (doRequest RequestCheapestDessert) defaultInventory
> test02 = runState (doRequest (RequestSpecificDessert "Apple Pie")) defaultInventory

</div>

Let's check if this works:

    *Main> runState (doRequest RequestCheapestDessert) defaultInventory
    (Just "Pancake",["Apple Pie","Apple Pie","Tiramisu"])

    *Main> runState
        (doRequest (RequestSpecificDessert "Apple Pie")) defaultInventory
    (Just "Apple Pie",["Pancake","Apple Pie","Tiramisu"])

Looking good so far!

Because our restaurant wants to make as much money as possible, we choose to
first serve the people who order a specific dessert. In order to do that, we
have a 'Priority' type and each kind of request maps to a priority. Lower
numbers means higher priority.

> type Priority = Int

> requestPriority :: Request -> Priority
> requestPriority (RequestSpecificDessert _) = 0
> requestPriority RequestCheapestDessert     = 1

Now let's see what happens when a bunch of friends visit our restaurant.

> friendsRequests :: [Request]
> friendsRequests =
>     [ RequestCheapestDessert
>     , RequestSpecificDessert "Apple Pie"
>     , RequestCheapestDessert
>     , RequestSpecificDessert "Pancake"
>     , RequestSpecificDessert "Crème brûlée"
>     ]

Easy: we first sort the requests by priority, and then we apply `doRequest` on
every `Request`. We keep the requests so we know which `Dessert` corresponds to
which `Request`.

> doRequests :: [Request] -> State Inventory [(Request, Maybe Dessert)]
> doRequests requests =
>     forM (sortBy (comparing requestPriority) requests) $
>         \req -> (,) req <$> doRequest req

Let's run this for our example to see if it worked and if we got the priorities
right:

<div style="display: none">

> test03 = runState (doRequests friendsRequests) defaultInventory

</div>

    *Main> runState (doRequests friendsRequests) defaultInventory
    ( [ (RequestSpecificDessert "Apple Pie",    Just "Apple Pie")
      , (RequestSpecificDessert "Pancake",      Just "Pancake")
      , (RequestSpecificDessert "Crème brûlée", Nothing)
      , (RequestCheapestDessert,                Just "Apple Pie")
      , (RequestCheapestDessert,                Just "Tiramisu")
      ]
    , []
    )

Works great! However, it gets trickier. What if, instead of just a list, we have
something with a bit more structure:

> data Family a = Family
>     { familyParent1  :: a
>     , familyParent2  :: a
>     , familyChildren :: V.Vector a
>     } deriving (Show)

And we want to implement:

> doFamilyRequests
>     :: Family Request -> State Inventory (Family (Maybe Dessert))
> doFamilyRequests = error "Implement me"

How do we go about that? Instead of just sorting by priority, we need to tag
which request belongs to which parent or child, then sort them, and... it gets
messy -- especially if the problem becomes more complicated. Imagine, for
example, that children get given a bit more priority. It would be cool if we
could *separate* the evaluation order (priority) from our actual logic.

Fortunately, there is an Applicative Functor which solves exactly this problem.

The Prio Applicative
====================

The `Prio` Applicative has three type parameters:

- `p`: The priority type, typically something like `Int` or `Double`;
- `m`: The Monad we are annotating with priorities, for example
  `State Inventory`;
- `a`: Our result type.

We use a [GADT] which mirrors the interface of Applicative, and one additional
constructor, which holds a monadic action together with its priority.

[GADT]: https://wiki.haskell.org/Generalised_algebraic_datatype

> data Prio p m a where
>     Pure :: a -> Prio p m a
>     App  :: Prio p m (a -> b) -> Prio p m a -> Prio p m b

>     Prio :: p -> m a -> Prio p m a

For reference, here is the interface of Applicative again:

~~~~~{.haskell}
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
~~~~~

We can define a functor instance in terms of Applicative:

> instance Functor (Prio p m) where
>     fmap f = App (Pure f)

And we can use the constructors to implement the Applicative instance:

> instance Applicative (Prio p m) where
>     pure  = Pure
>     (<*>) = App

Now, we probably want to hide the actual constructors from the users and just
provide a simple interface. Our interface consists of three functions:

- `prio` annotates a monadic action with a priority;
- `modifyPrio` modifies the priorities in a `Prio` value;
- `runPrio` evaluates the `Prio` to the base Monad.

The implementation of `prio` is straightforward:

> prio :: p -> m a -> Prio p m a
> prio = Prio

A simple implementation of `modifyPrio` walks through the tree and modifies
priorities (`Prio` nodes) as it encounters them [^modifyPrio].

[^modifyPrio]: A faster (but less concise) implementation would be to add a
`ModifyPrio` constructor, and evaluate all of these at once, so we only have to
go through the tree once.

> modifyPrio :: forall p m a. (p -> p) -> Prio p m a -> Prio p m a
> modifyPrio f = go
>   where
>     go :: forall b. Prio p m b -> Prio p m b
>     go (Pure x)   = Pure x
>     go (App x y)  = App (go x) (go y)
>     go (Prio p x) = Prio (f p) x

`runPrio` also has a simple implementation: we find the minimal priority, and
then evaluate all actions having this priority. When no priorities are left, we
can use `unsafeEvaluate` to evaluate the whole tree [^runPrio].

[^runPrio]: This implementation is very slow (quadratic in terms of the number
of nodes in the `Prio` "tree"). I have found a faster way to implement this, but
it is again less concise and requires the use of `unsafeCoerce`, so it is
omitted from this blogpost.

> runPrio :: (Monad m, Ord p) => Prio p m a -> m a
> runPrio os = case findMinimalPriority os of
>     Just p  -> evaluatePriority p os >>= runPrio
>     Nothing -> return $ unsafeEvaluate os

The three auxiliary functions used here `findMinimalPriority`,
`evaluatePriority` and `unsafeEvaluate` should be hidden from the user-facing
API (except perhaps `findMinimalPriority`). Let's look at how these functions
are implemented next.

`findMinimalPriority` simply goes through the `Prio` value and returns the
minimal priority.

> findMinimalPriority
>     :: forall p m a. (Monad m, Ord p)
>     => Prio p m a -> Maybe p
> findMinimalPriority = go Nothing
>   where
>     go :: forall b. Maybe p -> Prio p m b -> Maybe p
>     go !acc        (Pure _)   = acc
>     go !acc        (App x y)  = go (go acc x) y
>     go !Nothing    (Prio p _) = Just p
>     go !(Just !p0) (Prio p _) = Just (min p0 p)

`evaluatePriority` evaluates all nodes with a priority equal or less than the
given priority. We do so by replacing this `Prio` constructor by a `Pure`
constructor.

> evaluatePriority
>     :: forall p m a. (Monad m, Ord p)
>     => p -> Prio p m a -> m (Prio p m a)
> evaluatePriority p0 = go
>   where
>     go :: forall b. Prio p m b -> m (Prio p m b)
>     go (Pure x)     = return (Pure x)
>     go (App x y)    = liftM2 App (go x) (go y)
>     go (Prio p f)
>         | p <= p0   = liftM Pure f
>         | otherwise = return (Prio p f)

After we have recursively called `findMinimalPriority` and `evaluatePriority`
until all the `Prio` nodes are gone, we can call `unsafeEvaluate` to get our
actual value out.

> unsafeEvaluate :: Prio p m a -> a
> unsafeEvaluate (Pure x)   = x
> unsafeEvaluate (App x y)  = (unsafeEvaluate x) (unsafeEvaluate y)
> unsafeEvaluate (Prio _ _) = error
>     "unsafeEvaluate: internal error: some steps still unevaluated"

Usage example
=============

We can now try this out. Remember the type of `doRequest`:

~~~~~{.haskell}
doRequest :: Request -> State Inventory (Maybe Dessert)
~~~~~

Let's add a variant which uses the priority of the `Request`:

> prioRequest :: Request -> Prio Priority (State Inventory) (Maybe Dessert)
> prioRequest req = prio (requestPriority req) (doRequest req)

And for the whole family:

> prioFamilyRequests
>     :: Family Request
>     -> Prio Priority (State Inventory) (Family (Maybe Dessert))
> prioFamilyRequests family = Family
>     <$> prioRequest (familyParent1 family)
>     <*> prioRequest (familyParent2 family)
>     <*> (modifyPrio (\x -> x - 1) $
>             traverse prioRequest (familyChildren family))

Ain't that clean code. Let's test it out:

> familyRequest :: Family Request
> familyRequest = Family
>     { familyParent1  = RequestCheapestDessert
>     , familyParent2  = RequestSpecificDessert "Apple Pie"
>     , familyChildren = V.fromList
>           [ RequestCheapestDessert
>           , RequestSpecificDessert "Pancake"
>           , RequestSpecificDessert "Crème brûlée"
>           ]
>     }

<div style="display: none">

> test04 = runState (runPrio $ prioFamilyRequests familyRequest) defaultInventory

</div>

    *Main> runState (runPrio $ prioFamilyRequests familyRequest)
                    defaultInventory
    ( Family
        { familyParent1  = Just "Tiramisu"
        , familyParent2  = Just "Apple Pie"
        , familyChildren = fromList
            [ Just "Apple Pie"
            , Just "Pancake"
            , Nothing
            ]
        }
    , []
    )

Correct!

Conclusion
==========

`Prio` is an interesting Applicative. I particularly like the fact that it works
for every Monad (although it doesn't make sense for some Monads such as
`Reader`).

Use cases are rare. I've only encountered one and I could also have implemented
it in a different way (although this feels a lot cleaner). However, I think a
really important point about it is that it really illustrates the difference
between Applicative and Monad very well.

Thanks to Alex Sayers, Jared Tobin and Maciej Wos for proofreading and
discussions.
