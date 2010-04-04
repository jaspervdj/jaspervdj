---
title: Google AI Challenge and Haskell list performance
description: A post about a typical issue with Haskell lists.
tags: haskell, code, ai
---

<h2>What is this about</h2>

This blogpost is about the
[Google AI Challenge](http://csclub.uwaterloo.ca/contest/index.php), which
allows Haskell as a possible language one can write a bot in. Unfortunately,
the code they provide for Haskell is very inefficient. Because the contest
requires you to do all your calculations in one second, this is quite a setback.
This post is literate Haskell, so you should basically be able to paste it into
a `.lhs` file and run it.

> module Main where

First, we need some imports. You can safely ignore these, but unfortunately the
compiler cannot.

> import Control.Monad (replicateM, forever)
> import qualified Data.Array.Unboxed as UA
> import Data.Array.Unboxed ((!))
> import Data.List (transpose)
> import System.IO (hSetBuffering, BufferMode(LineBuffering), stdin, stdout)

We start by defining a `Tile` type. Tiles are our points on the 2D map, so we
can easily represent them as tuples:

> type Tile = (Int, Int)

This allows to write, for example, the code to determine the adjacent tiles in
a very clear and concise way.

> adjacentTiles :: Tile -> [Tile]
> adjacentTiles (x, y) = [ (x, y - 1)
>                        , (x + 1, y)
>                        , (x, y + 1)
>                        , (x - 1, y)
>                        ]

But that is not what this article is about. The thing is that in the code they
provided as a starter package, they stored the information about the map in a
`[[Spot]]` type -- a matrix of `Spot`s, where a `Spot` would be either a wall,
an empty tile, the player...

The problem with this is that Haskell's default lists are linked lists. Imagine
we have a _m * n_ map -- so _m_ tiles wide and _n_ tiles high. In most other
languages, you would expect you can access the elements in constant time --
so _O(1)_. But because we're using linked lists, this is not the case. Worst
case scenario, you would access the bottom right element. This would take
_O(m + n)_ time -- and that is not acceptable.

So I'll abuse this blogpost to ramble about Haskell arrays.

> data Board = Board
>     { boardWidth :: Int
>     , boardHeight :: Int
>     , boardWalls :: UA.UArray (Int, Int) Bool
>     , boardBotPosition :: (Int, Int)
>     , boardEnemyPosition :: (Int, Int)
>     }

We use an _Unboxed Immutable Array_ to store or walls. A bit of context:

- __Unboxed__: this is basically a counter-measure against Haskell's laziness.
  While that can be very useful in certain situations, it is not here. By using
  an unboxed array, we force the contents to be fully evaluated. This means the
  array will not take up much more room than the corresponding array in C would.
- __Immutable__: because we read the `Board` again every time -- that's the way
  the engine works -- we don't have to change the board at all. This means we
  can simply write nice, purely functional code here.

The code to load this `Board` follows later -- let's first see how we lookup a
certain `Tile` in the `Board`:

> isWall :: Board -> (Int, Int) -> Bool
> isWall board (x, y) =  x < 0 || x >= boardWidth board
>                     || y < 0 || y >= boardHeight board
>                     || boardWalls board ! (x, y)

Yep, we now have a function that runs in _O(1)_ and that is quite efficient (of
course we have to do some basic boundary checks). The code to read the `Board`
is a little more complicated (but also more interesting):

> readBoard :: Int -> Int -> [String] -> Board
> readBoard width height lines = Board
>     { boardWidth = width
>     , boardHeight = height
>     , boardWalls = walls
>     , boardBotPosition = find '1'
>     , boardEnemyPosition = find '2'
>     }

The width and height are passed to the function, along with a number of lines.
The real code happens in the `where` block:

>   where
>     string = concat $ transpose $ map (take width) lines
>     matrix = UA.listArray ((0, 0), (width - 1, height - 1)) string
>     walls = UA.amap (/= ' ') matrix
>     find c = head [ i | i <- UA.indices matrix, matrix ! i == c ]

What might not be obvious to see, is why we use `transpose` here. `transpose`
transposes a matrix -- why would we want that?

The answer is that I find it easier (if you will, more _natural_) to use
`(x, y)` notation instead of `(row, column)` notation. You can safely remove
the `transpose` function and switch to `(row, column)` if you want, but I
think the `(x, y)` notation is a little easier.

We also consider everything that is not a space character a wall -- that
includes the player and the enemy. I do this because this makes my code easier
(and it makes sense in a way, you don't want to steer through yourself or the
enemy).

Then we use a small `find` function to locate the player and the enemy in the
array. This find functions should run very fast, since looking up in arrays is,
well, very fast[^1].

[^1]: This function originally was a little longer, thanks to Boris Lykah for
      correcting this.

We call this `readBoard` function more or less in the following way:

> takeTurn :: IO ()
> takeTurn = do
>     line <- getLine
>     let [width, height] = map read $ words line
>     lines <- replicateM height getLine
>     let board = readBoard width height lines
>     -- Select tile to go next.
>     -- Print out new direction to stdout.
>     putStrLn "1"

So we can make our main very short:

> main :: IO ()
> main = do hSetBuffering stdin LineBuffering
>           hSetBuffering stdout LineBuffering
>           forever takeTurn

Note that the only reason our `main` function is so small is because I currently
still have a "stateless" bot. We also have to set line buffering[^2] for the
contest. I hope I can keep it that way, we'll see how the contest advances.

[^2]: I originally forgot this, kudos to Frank and w84 for noticing this and
      letting me know.
