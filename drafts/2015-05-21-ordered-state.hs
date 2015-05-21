{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Applicative (Applicative (..), (<$>))
import           Control.Monad.State (State, evalState, state)
import           Data.Traversable    (for)

--------------------------------------------------------------------------------
-- Example program written against 'OrderedState'.

-- | A dessert.
type Dessert = String

-- | List of available desserts.
type Inventory = [Dessert]

-- | Lower is higher priority.
type Priority = Int

-- | Sorted from cheapest to most expensive.
myInventory :: Inventory
myInventory =
    [ "Pancake"
    , "Apple Pie"
    , "Apple Pie"
    , "Tiramisu"
    ]

-- | A client can request either a specific dessert, or just go with the
-- cheapest.
data Request
    = RequestSpecificDessert Dessert
    | RequestCheapestDessert
    deriving (Show)

-- | Since we prefer making a lot of money, we serve clients with a specific
-- request first.
requestPriority :: Request -> Priority
requestPriority (RequestSpecificDessert _) = 0
requestPriority RequestCheapestDessert     = 1

-- | My requests.
myRequests :: [Request]
myRequests =
    [ RequestCheapestDessert
    , RequestSpecificDessert "Apple Pie"
    , RequestCheapestDessert
    , RequestSpecificDessert "Pancake"
    , RequestSpecificDessert "Crème brûlée"
    ]

-- | Given an 'Inventory' in the 'State' monad, perform a single request.
doRequest :: Request -> State Inventory (Maybe Dessert)
doRequest RequestCheapestDessert =
    -- The first item in the inventory is always the cheapest.
    state $ \inventory -> case inventory of
        []             -> (Nothing, [])
        (dessert : xs) -> (Just dessert, xs)
doRequest (RequestSpecificDessert requested) =
    -- Use `break` to take out the requested item.
    state $ \inventory -> case break (== requested) inventory of
        (xs, dessert : ys) -> (Just dessert, xs ++ ys)
        (xs, [])           -> (Nothing, xs)

-- | Order the requests by priority in the 'OrderedState' applicative.
orderRequests
    :: [Request]
    -> OrderedState Priority Inventory [(Request, Maybe Dessert)]
orderRequests requests = for requests $ \req ->
    prio (requestPriority req) $ (,) req <$> doRequest req

-- | Utility for evaluating the example.
myResult :: [(Request, Maybe Dessert)]
myResult =
    let s = runOrderedState (orderRequests myRequests)
    in evalState s myInventory

--------------------------------------------------------------------------------
-- Implementation of 'OrderedState'

data OrderedState p s a where
    Pure :: a -> OrderedState p s a
    App  :: OrderedState p s (a -> b)
         -> OrderedState p s a
         -> OrderedState p s b

    Prio :: p -> State s a -> OrderedState p s a

instance Functor (OrderedState p s) where
    fmap f = App (Pure f)

instance Applicative (OrderedState p s) where
    pure  = Pure
    (<*>) = App

prio :: p -> State s a -> OrderedState p s a
prio = Prio

runOrderedState :: Ord p => OrderedState p s a -> State s a
runOrderedState os = case findMinimalPriority os of
    Just p  -> evaluatePriority p os >>= runOrderedState
    Nothing -> return $ unsafeEvaluate os

unsafeEvaluate :: OrderedState p s a -> a
unsafeEvaluate (Pure x)   = x
unsafeEvaluate (App x y)  = (unsafeEvaluate x) (unsafeEvaluate y)
unsafeEvaluate (Prio _ _) = error
    "unsafeEvaluate: internal error: some steps still unevaluated"

evaluatePriority
    :: forall p s a. Ord p
    => p -> OrderedState p s a -> State s (OrderedState p s a)
evaluatePriority p0 = go
  where
    go :: forall b. OrderedState p s b -> State s (OrderedState p s b)
    go (Pure x)     = return (Pure x)
    go (App x y)    = App <$> go x <*> go y
    go (Prio p f)
        | p <= p0   = Pure <$> f
        | otherwise = return (Prio p f)

findMinimalPriority :: forall p s a. Ord p => OrderedState p s a -> Maybe p
findMinimalPriority = go Nothing
  where
    go :: forall b. Maybe p -> OrderedState p s b -> Maybe p
    go !acc        (Pure _)   = acc
    go !acc        (App x y)  = go (go acc x) y
    go !Nothing    (Prio p _) = Just p
    go !(Just !p0) (Prio p _) = Just (min p0 p)
