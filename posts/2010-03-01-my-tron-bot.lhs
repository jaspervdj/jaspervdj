---
title: My Haskell TronBot for the Google AI Challenge
description: My bot turned out to be the top scoring Haskell bot, so here's the code
tags: haskell, code, ai
---
This is the code for my entry in the
[Google AI Challenge](http://csclub.uwaterloo.ca/contest/index.php) 2010.
It turned out to be the best
[Belgian](http://csclub.uwaterloo.ca/contest/country_profile.php?country_id=32)
and the best
[Haskell](http://csclub.uwaterloo.ca/contest/language_profile.php?lang=Haskell)
bot ([screenshot]($root/images/2010-03-01-final-rankings.png)), so I thought
some people might be interested in the code. Luckily, I have been writing this
bot in Literate Haskell since the beginning, for a few reasons:

- I always wanted to try Literate Haskell for something "more serious".
- This will force me to keep the code more or less readable and clean.
- I am going to keep the code in one file, so it's quite easy to maintain as
  well.

> module Main where

This is the actual code of my bot, as submitted in the contest. The only changes
made after the deadline are:

- Cleanup (mostly removing `Debug.trace` statements).
- Adding more explanations and comments.

Anyway, some disclaimers.

Disclaimer 1: This is quite a large source file/blogpost. If you're not
interested at all, this could be a boring read.

Disclaimer 2: This code is unfinished. There are some situations in which this
bot will make very bad decisions. Possibly, there are situations that can crash
him, which leads us to disclaimer 3.

Disclaimer 3: I cannot be held responsible if my bot initiates a nuclear attack
in an attempt to wipe out the human race.

Before we begin, our Bot uses three important strategies:

- Evaluation of a [minimax game tree](http://en.wikipedia.org/wiki/Minimax),
  including (deep)
  [Alpha-beta pruning](http://en.wikipedia.org/wiki/Alpha-beta_pruning).
- [Iterative deepening](http://en.wikipedia.org/wiki/Iterative_deepening) to
  stay within our time limit.
- A [flood fill](http://en.wikipedia.org/wiki/Flood_fill)-based technique to
  determine space left.

We start, as always, by importing the needed modules.

> import System.Time
> import System.IO
> import Control.Monad
> import Control.Applicative ((<$>))
> import Data.Ord (comparing)
> import Data.List (transpose, nub, sortBy)
> import qualified Data.Set as S
> import Data.Set (Set, (\\))
> import qualified Data.Array.Unboxed as UA
> import Data.Array.Unboxed ((!))
> import Control.Concurrent
> import Data.Maybe (fromJust, isNothing, isJust, listToMaybe)

To represent `Tile`s -- positions on the 2D grid -- we use a simple tuple.

> type Tile = (Int, Int)

Because of this, we can write a very concise function to find the adjacent
tiles. As you can see, I inline this function here to avoid some overheads. I
use this on several places throughout the code.

> adjacentTiles :: Tile -> [Tile]
> adjacentTiles (x, y) = [ (x, y - 1)
>                        , (x + 1, y)
>                        , (x, y + 1)
>                        , (x - 1, y)
>                        ]
> {-# INLINE adjacentTiles #-}

There's a simple formula for calculating the Pythagorean distance between
tiles. We use this in combination with the "real" distance (the distance when
taking walls etc. into account). We can leave this distance squared, not taking
the `sqrt` is a little faster, and we only have to compare distances, and
`x^2 < y^2` implies `x < y`, because distances are always non-negative.

> distanceSquared :: Tile -> Tile -> Int
> distanceSquared (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

Now, we need a data structure for the board.

> data BoardBase = BoardBase { baseWidth         :: Int
>                            , baseHeight        :: Int
>                            , baseWalls         :: UA.UArray Tile Bool
>                            , baseBotPosition   :: Tile
>                            , baseEnemyPosition :: Tile
>                            } deriving (Show)

However, we also want a board structure that can be adjusted very quickly. We
will therefore wrap the `BoardBase` in another structure, capable of making a
few quick adjustments.

This `Board` will be used to consider "possible" moves. When we make a move on
this `Board` type, we just have to add a wall to the `Set`, and an element to
the list of positions.

> data Board = Board { boardBase              :: BoardBase
>                    , boardAdditionalWalls   :: Set Tile
>                    , boardBotPositionList   :: [Tile]
>                    , boardEnemyPositionList :: [Tile]
>                    } deriving (Show)

We can create a `Board` from a `BoardBase` very quickly.

> boardFromBase :: BoardBase -> Board
> boardFromBase base = Board { boardBase = base
>                            , boardAdditionalWalls = S.empty
>                            , boardBotPositionList = [baseBotPosition base]
>                            , boardEnemyPositionList = [baseEnemyPosition base]
>                            }

Now, let's define some easy functions we can apply on a `Board`. The positions
of the bot and the enemy are determined by the last move added to their list of
moves.

> boardBotPosition :: Board -> Tile
> boardBotPosition = head . boardBotPositionList
> {-# INLINE boardBotPosition #-}

> boardEnemyPosition :: Board -> Tile
> boardEnemyPosition = head . boardEnemyPositionList
> {-# INLINE boardEnemyPosition #-}

Later on, we will construct "possible" next `Board`s. Given such a `Board`, we
want to determine the first move our Bot made, since that would be the move our
AI will choose. This might give no result, so we wrap it in a `Maybe` type.

> boardBotMove :: Board -> Maybe Tile
> boardBotMove = listToMaybe . tail . reverse . boardBotPositionList

Checking if a certain tile is a wall is quite simple -- but we need to remember
we also have to check the additional walls in the `Board`. We first check for
boundaries to prevent errors, then we check in the `walls` first, because
`Array` access is faster than `Set` access here. Also, we __really__ want to
inline this function, because it is called over 9000 times.

> isWall :: Board -> Tile -> Bool
> isWall board tile@(x, y) =  x < 0 || x >= baseWidth base
>                          || y < 0 || y >= baseHeight base
>                          || (baseWalls base) ! tile
>                          || tile `S.member` boardAdditionalWalls board
>   where
>     base = boardBase board
> {-# INLINE isWall #-}

What now follows is the function with which we read the `Board` from a number of
lines. This is quite boring code, so you can safely skip it.

> readBoard :: Int -> Int -> [String] -> Board
> readBoard width height lines' = boardFromBase
>     BoardBase { baseWidth         = width
>               , baseHeight        = height
>               , baseWalls         = walls
>               , baseBotPosition   = find '1'
>               , baseEnemyPosition = find '2'
>               }
>   where
>     string = concat $ transpose $ map (take width) lines'
>     matrix = UA.listArray ((0, 0), (width - 1, height - 1)) string
>     walls = UA.amap (== '#') matrix
>     find c = head [ i | i <- UA.indices matrix, matrix ! i == c ]

Although we do not use the next function in the AI, it is quite handy for
testing reasons, when playing around with `ghci`.

> readBoardFromFile :: FilePath -> IO Board
> readBoardFromFile file = do
>     h <- openFile file ReadMode
>     line <- hGetLine h
>     let [width, height] = map read $ words line
>     contents <- hGetContents h
>     return $ readBoard width height (lines contents)

Next is a function to inspect the entire `Board`, to determine it's value later
on. It uses a flood fill based approach.

It starts one flood fill starting from the enemy, and one starting from our bot.
This determines the space left for each combatant. Also, when the two flood
meet, we have found a path between them. This double flood fill is illustrated
here in this animation:

![Flood fill illustration]($root/images/2010-03-01-flood-fill.gif)

The function returns:

- The free space around the bot.
- The free space around the enemy.
- The distance between the bot and the enemy, or `Nothing` if there is no path
  from the bot to the enemy.

> inspectBoard :: Board -> (Int, Int, Maybe Int)
> inspectBoard board =
>     floodFill (S.singleton botPosition, S.singleton enemyPosition)
>               S.empty (0, 0) 0 Nothing
>   where
>     botPosition = boardBotPosition board
>     enemyPosition = boardEnemyPosition board

Because we have to make all decisions in under one second, we have a depth limit
for our flood fill. When this limit is reached, the result will be the same as
if we encountered walls in all directions.

>     maxDepth = 25

This is a small auxiliary function that extends `isWall` with the enemy and
bot positions - we do not want to fill any of those two positions.

>     isBlocked tile = isWall board tile
>                    || tile == botPosition
>                    || tile == enemyPosition

This is the main queue-based flood fill function. It's arguments are:

- A tuple of two `Set`s, containing the neighbour tiles of the bot's flood,
  and those of the enemy's flood.
- A `Set` of already flooded tiles.
- A tuple of two `Int`s, containing the number of tiles filled by the bot, and
  the number of tiles filled by the enemy.
- The current depth of our search.
- The best distance from the bot to the enemy, or `Nothing` if not found yet.

It works in a fairly straightforward recursive way.

>     floodFill :: (Set Tile, Set Tile) -> Set Tile
>               -> (Int, Int) -> Int -> Maybe Int -> (Int, Int, Maybe Int)
>     floodFill (neighbours1, neighbours2) set
>               (fill1, fill2) currentDepth currentDistance

If we reached our search limit, or we have no more tiles to inspect, we just
return what we currently have.

>         | (S.null neighbours1 && S.null neighbours2)
>             || maxDepth <= currentDepth = (fill1, fill2, currentDistance)

Otherwise, we expand our search.

>         | otherwise =
>             floodFill (validNext1, validNext2) newSet
>                       (fill1 + S.size validNext1, fill2 + S.size validNext2)
>                       (currentDepth + 1) distance
>       where

The next tiles to add are those adjacent to the current neighbours of the flood.

>         getNext = S.filter (not . isBlocked) . S.fromList
>                 . concatMap adjacentTiles . S.toList
>
>         next1 = getNext neighbours1
>         next2 = getNext neighbours2

If the enemy is in the next set of neighbours, we have found our distance. If
there is no intersection at all, we haven't reached the enemy yet. We have to
do a little trickery here, because the distance could be even or odd.

>         odd' = if S.null (next1 `S.intersection` next2)
>                    then Nothing
>                    else Just (2 * currentDepth + 1)
>         even' = if S.null (next1 `S.intersection` neighbours2) &&
>                    S.null (next2 `S.intersection` neighbours1)
>                     then Nothing
>                     else Just (2 * currentDepth)
>         distance = currentDistance `mplus` odd' `mplus` even'

Now, we enlarge our `Set` of already added tiles and remove them from the next
tiles to add (since they are already added). We also filter out the
non-accessible `Tile`s, and we make sure no `Tile`s appear in both `validNext1`
and `validNext2`.

>         newSet = set `S.union` neighbours1 `S.union` neighbours2
>         validNext1 = next1 \\ newSet
>         validNext2 = next2 \\ newSet

The algorithm needs to be able to determine the "best" choice in some way or
another. So we need to be able compare two games. To make this easier, we can
assign a `Score` to a game - and we then make these `Score`s comparable.

> data Score = Win
>            | Loss
>            | Draw

In theory, these are the only possible outcomes. In reality, these values are
often situated at the bottom of our game tree -- and we can't look down all the
way. Therefore, we also have a `Game` score -- describing a game in progress.

The `Game` constructor simply holds some fields so we can determine it's value:

- Free space for the bot, as determined by a flood fill.
- Free space for the enemy, as determined by a flood fill.
- `Just d` if `d` is the distance to the enemy. If the enemy cannot be reached,
  or is to far away to be detected, this will be `Nothing`.
- The number of adjacent walls next to the bot, after the first move.
- The Pythagorean distance to the enemy (squared).

>            | Game Int Int (Maybe Int) Int Int
>            deriving (Eq, Show)

To choose the best game, we need a way to compare games. That's why we implement
the `Ord` class. A win is always the best, and a loss is always the worse.

> instance Ord Score where
>     Win  <= _    = False
>     Loss <= _    = True

We only see a draw as worse if our bot would have less space otherwise. This is
a quite pessimistic view, but well, we can't risk too much.

>     Draw <= (Game botSpace enemySpace _ _ _) =
>          botSpace >= enemySpace

Comparing two games is harder, since we have to make "guesses" here.

>     (Game bs1 es1 ds1 aw1 pd1) <= (Game bs2 es2 ds2 aw2 pd2)

When there is a space difference: choose direction with most free space.

>         | sp1 /= sp2 = sp1 <= sp2

When the enemy is not reachable: choose direction with most adjacent walls, as
this fills our space quite efficiently.

>         | isNothing ds1 && isNothing ds2 = aw1 <= aw2

When the enemy is reachable from both situations, we choose smallest distance.
First we try the "real" distance, then the Pythagorean distance.

>         | isJust ds1 && isJust ds2 =
>             if ds1 /= ds2 then fromJust ds1 >= fromJust ds2
>                           else pd1 >= pd2

Now, there are some edge cases left. We prefer to create situations were we
"lock up" the other bot, but only if it means we have more space than the other
bot.

>         | isJust ds1 && isNothing ds2 = bs2 >= es2
>         | isNothing ds1 && isJust ds2 = bs1 >= es1
>       where

The free space mentioned is determined as the bot space minus the enemy space.

>         sp1 = bs1 - es1
>         sp2 = bs2 - es2

We're not going to write everything twice, so if pattern matching failed, try
the other way around:

>     a <= b = b >= a

Okay, when building our game tree, we need to find out if a certain node in the
Alpha-Beta tree is a leaf. A leaf means the game ends - so there's either a
collision, or a draw.

> gameIsLeaf :: Board -> Bool
> gameIsLeaf board =  botPosition == enemyPosition
>                  || isWall board botPosition
>                  || isWall board enemyPosition
>   where
>     botPosition = boardBotPosition board
>     enemyPosition = boardEnemyPosition board

If the game is a leaf, the value is trivial to determine:

> gameLeafValue :: Board -> Score
> gameLeafValue board
>     | botPosition == enemyPosition = Draw
>     | botCrashed && enemyCrashed   = Draw
>     | botCrashed                   = Loss
>     | enemyCrashed                 = Win
>     | otherwise                    = error "Not a leaf node."
>   where
>     botPosition = boardBotPosition board
>     enemyPosition = boardEnemyPosition board
>     botCrashed = isWall board botPosition
>     enemyCrashed = isWall board enemyPosition

If the game is not a leaf, we have to make an estimate of the value. This is
basically just calling some functions to fill in the fields of the `Game`
constructor of `Score`.

> gameNodeValue :: Board -> Score
> gameNodeValue board =
>     Game botSpace enemySpace distance numberOfAdjacentWalls distanceSquared'
>   where
>     botPosition = boardBotPosition board
>     enemyPosition = boardEnemyPosition board
>
>     distanceSquared' = distanceSquared botPosition enemyPosition
>
>     allAdjacent = nub $ adjacentTiles =<< boardBotPositionList board
>     numberOfAdjacentWalls = length (filter (isWall board) allAdjacent)
>
>     (botSpace, enemySpace, distance) = inspectBoard board

Now, we have a function to create the child values of a node in the game tree.
This function creates 4 new boards, with all the directions the bot (or the
enemy, if `isBot` is `False`) can move to.

> gameNodeChildren :: Board -> Bool -> [Board]
> gameNodeChildren board isBot = do
>     adjacent <- adjacentTiles position
>     if isBot
>         then return board
>             { boardAdditionalWalls   = walls
>             , boardBotPositionList   = adjacent : botPositionList
>             , boardEnemyPositionList = enemyPositionList
>             }
>         else return board
>             { boardAdditionalWalls   = walls
>             , boardBotPositionList   = botPositionList
>             , boardEnemyPositionList = adjacent : enemyPositionList
>             }
>   where
>     position = (if isBot then boardBotPosition else boardEnemyPosition) board
>     walls = position `S.insert` boardAdditionalWalls board
>
>     botPositionList = boardBotPositionList board
>     enemyPositionList = boardEnemyPositionList board

We now have our main minimax search function. The `maxDepth` argument
gives us a depth limit for our search, and also indicates if it's our turn or
the enemy's turn (it's our turn when it's even, enemy's turn when it's odd).

The `contact` argument tells us if there is a way for our bot to reach the
enemy. If the enemy cannot be reached, we do not have to consider it's turns,
sparing us some valuable resources.

This function uses a simple form of (deep) Alpha-beta pruning. I'm pretty sure
`botSearch` and `enemySearch` could be written as one more abstract function,
but I think it's pretty clear now, too.

For one unfamiliar with minimax trees or Alpha-beta pruning, this function
simply returns the possible `Board` with the best `Score`.

> searchGameTree :: Board -> Int -> Bool -> (Score, Score) -> (Board, Score)
> searchGameTree parent maxDepth contact (lower, upper)
>     | gameIsLeaf parent && isBot = (parent, gameLeafValue parent)
>     | maxDepth <= 0 = (parent, gameNodeValue parent)
>     | otherwise =
>          if isBot then botSearch children (lower, upper) parent
>                   else enemySearch children (lower, upper) parent
>   where
>     isBot = maxDepth `mod` 2 == 0
>     children = gameNodeChildren parent isBot
>
>     botSearch [] (l, _) current = (current, l)
>     botSearch (x : xs) (l, u) current =
>         let newDepth = if contact then maxDepth - 1 else maxDepth - 2
>             (board, value) = searchGameTree x newDepth contact (l, u)
>         in if value >= u
>                then (board, value)
>                else if value > l then botSearch xs (value, u) board
>                                  else botSearch xs (l, u) current
>
>     enemySearch [] (_, u) current = (current, u)
>     enemySearch (x : xs) (l, u) current =
>         let (board, value) = searchGameTree x (maxDepth - 1) contact (l, u)
>         in if value <= l
>                 then (board, value)
>                 else if value < u then enemySearch xs (l, value) board
>                                   else enemySearch xs (l, u) current

First, we want to make a quick (but stupid) decision, in case we're on a very
slow processor or if we don't get a lot of CPU ticks. The following function
does that, providing a simple "Chaser" approach.

> simpleDecision :: Board -> Tile
> simpleDecision board

In case we really have no valid options, we just go north.

>     | null valid = head directions

If there are some non-wall options, we pick the first one in the list, which
will conveniently be the `Tile` closest to the enemy.

>     | otherwise = head valid
>   where

We take the `adjacentTiles` of the `botPosition`, and sort them according to
distance to the enemy. That way, we get our "Chaser" behaviour.

>     botPosition = boardBotPosition board
>     directions = sortBy (comparing distance) $ adjacentTiles botPosition
>     valid = filter (not . isWall board) directions
>     distance = distanceSquared (boardEnemyPosition board)

The next function performs one turn, meaning:

- It reads the current board state from `stdin`.
- It builds a game tree and determines the best option.
- It prints that option back to `stdout`.

> takeTurn :: IO ()
> takeTurn = do

The first line tells us the board dimensions. We then take the next `height`
lines and read the board from it.

>     [width, height] <- (map read . words) <$> getLine
>     board <- readBoard width height <$> replicateM height getLine

We have an `MVar` to hold our decision. We begin by filling it by something
simple and then improve that simple result in another thread.

>     mvar <- newMVar $ simpleDecision board
>     calculationThread <- forkIO $ makeMinMaxDecision board mvar

We wait 900 ms. After that time has passed, our `MVar` should contain a
reasonably smart decision. We take it and finish off our calculation thread.

>     threadDelay $ 900 * 1000
>     result <- takeMVar mvar
>     killThread calculationThread

Now, all that is left is printing the direction our AI made.

>     putStrLn $ tileToDirection board result
>   where

This is the function that is executed in another thread. It simply tries to
calculate a smart decision using `minMaxDecision` and then puts it in the
`MVar`. We also do a simple `inspectBoard` to determine if there is a path
between us and the enemy.

It also uses a form of iterative deepening; first, the best decision for depth
2 is calculated. Then, we try to find the best decision for depth 4, then 6,
and so on. Tests seemed to show that it usually gets to depth 8 or 10 before
it is killed.

>     makeMinMaxDecision board mvar = makeMinMaxDecision' 2
>       where
>         (_, _, distance) = inspectBoard board
>         contact = isJust distance
>
>         makeMinMaxDecision' depth = do
>             let (best, _) = searchGameTree board depth contact (Loss, Win)
>                 move = boardBotMove best
>             when (isJust move) $ swapMVar mvar (fromJust move) >> return ()
>             makeMinMaxDecision' $ depth + 2

We need to determine the direction of a tile from it's coordinates, because the
game engine is expecting a direction - and we only have a `Tile`.

>     tileToDirection board position
>         | position == (x, y - 1) = "1"
>         | position == (x + 1, y) = "2"
>         | position == (x, y + 1) = "3"
>         | position == (x - 1, y) = "4"
>         | otherwise              = "Error: unknown move."
>       where
>         (x, y) = boardBotPosition board

Our main function must set the correct buffering options and then loop forever.
At this point, it's very cool we have a stateless bot, since we can now just
`forever takeTurn`.

> main :: IO ()
> main = do
>     hSetBuffering stdin LineBuffering
>     hSetBuffering stdout LineBuffering
>     forever takeTurn

An auxiliary function to help timing. We only take the seconds and the
picoseconds into account, because when we're taking more than minutes, well,
we're fucked anyway.

> toMs :: ClockTime -> ClockTime -> Int
> toMs t1 t2 = let d = diffClockTimes t2 t1
>              in tdSec d * 1000 + fromIntegral (tdPicosec d `div` 1000000000)

That's it. As always, all criticism and questions are welcome. By the way, you
can find the `.lhs` file
[here](http://github.com/jaspervdj/jaspervdj/raw/master/posts/2010-03-01-my-tron-bot.lhs).
