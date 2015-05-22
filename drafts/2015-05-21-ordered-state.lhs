> {-# LANGUAGE BangPatterns        #-}
> {-# LANGUAGE GADTs               #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> import           Control.Applicative (Applicative (..), (<$>))
> import           Control.Monad       (forM, liftM, liftM2)
> import           Control.Monad.State (State, evalState, runState, state)
> import           Data.Foldable       (Foldable (..))
> import           Data.Monoid         ((<>))
> import           Data.List           (sortBy)
> import           Data.Ord            (comparing)
> import           Data.Traversable    (Traversable (traverse), for)
> import qualified Data.Vector         as V

In our example, we will be modelling a dessert restaurant.

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

- Pick a specific dessert;
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


> -- TODO: Move me
> test01 = runState (doRequest RequestCheapestDessert) defaultInventory
> test02 = runState (doRequest (RequestSpecificDessert "Apple Pie")) defaultInventory

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
> doRequests requests = forM (sortBy (comparing requestPriority) requests) $
>     \req -> (,) req <$> doRequest req

> -- TODO: Fix result and move.
> test03 = runState (doRequests friendsRequests) defaultInventory

    *Main> runState (doRequests friendsRequests) defaultInventory
    ( [ Just "Apple Pie"
      , Just "Pancake"
      , Nothing
      , Just "Apple Pie"
      , Just "Tiramisu"
      ]
    , []
    )

> -- | Order the requests by priority in the 'Prio' applicative.
> orderRequests
>     :: [Request]
>     -> Prio Priority (State Inventory) [(Request, Maybe Dessert)]
> orderRequests requests = for requests $ \req ->
>     prio (requestPriority req) $ (,) req <$> doRequest req

> -- | Utility for evaluating the example.
> myResult :: [(Request, Maybe Dessert)]
> myResult =
>     let s = runPrio (orderRequests friendsRequests)
>     in evalState s defaultInventory

> data Family a = Family
>     { familyParent1  :: a
>     , familyParent2  :: a
>     , familyChildren :: V.Vector a
>     } deriving (Show)

> instance Functor Family where
>     fmap f family = Family
>         { familyParent1  = f       (familyParent1  family)
>         , familyParent2  = f       (familyParent2  family)
>         , familyChildren = V.map f (familyChildren family)
>         }

> instance Foldable Family where
>     foldMap f family =
>         f         (familyParent1  family) <>
>         f         (familyParent2  family) <>
>         foldMap f (familyChildren family)

> instance Traversable Family where
>     traverse f family = Family
>         <$> f          (familyParent1  family)
>         <*> f          (familyParent2  family)
>         <*> traverse f (familyChildren family)

> --------------------------------------------------------------------------------
> -- Implementation of 'Prio'

> data Prio p m a where
>     Pure :: a -> Prio p m a
>     App  :: Prio p m (a -> b) -> Prio p m a -> Prio p m b

>     Prio :: p -> m a -> Prio p m a

> instance Functor (Prio p m) where
>     fmap f = App (Pure f)

> instance Applicative (Prio p m) where
>     pure  = Pure
>     (<*>) = App

> prio :: p -> m a -> Prio p m a
> prio = Prio

> runPrio :: (Monad m, Ord p) => Prio p m a -> m a
> runPrio os = case findMinimalPriority os of
>     Just p  -> evaluatePriority p os >>= runPrio
>     Nothing -> return $ unsafeEvaluate os

> unsafeEvaluate :: Prio p m a -> a
> unsafeEvaluate (Pure x)   = x
> unsafeEvaluate (App x y)  = (unsafeEvaluate x) (unsafeEvaluate y)
> unsafeEvaluate (Prio _ _) = error
>     "unsafeEvaluate: internal error: some steps still unevaluated"

> evaluatePriority
>     :: forall p m a. (Monad m, Ord p) => p -> Prio p m a -> m (Prio p m a)
> evaluatePriority p0 = go
>   where
>     go :: forall b. Prio p m b -> m (Prio p m b)
>     go (Pure x)     = return (Pure x)
>     go (App x y)    = liftM2 App (go x) (go y)
>     go (Prio p f)
>         | p <= p0   = liftM Pure f
>         | otherwise = return (Prio p f)

> findMinimalPriority :: forall p m a. (Monad m, Ord p) => Prio p m a -> Maybe p
> findMinimalPriority = go Nothing
>   where
>     go :: forall b. Maybe p -> Prio p m b -> Maybe p
>     go !acc        (Pure _)   = acc
>     go !acc        (App x y)  = go (go acc x) y
>     go !Nothing    (Prio p _) = Just p
>     go !(Just !p0) (Prio p _) = Just (min p0 p)
