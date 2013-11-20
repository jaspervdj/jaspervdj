---
title: Picking a random element from a frequency list
description: A mildly interesting algorithm I used in Lorem Markdownum
tags: haskell
---

Introduction
============

A week or so ago, I wrote [Lorem Markdownum]: a small webapp to generate random
text (like the many [lorem ipsum] generators out there), but in [markdown]
format.

[Lorem Markdownum]: http://jaspervdj.be/lorem-markdownum/
[lorem ipsum]: http://www.lipsum.com/
[markdown]: http://daringfireball.net/projects/markdown/

This blogpost explains a mildly interesting algorithm I used to pick a random
element from a frequency list. It is written in Literate Haskell so you should
be able to drop it into a file and run it -- the raw version can be found
[here](TODO).

> import           Data.List       (sortBy)
> import qualified Data.Map.Strict as M
> import           System.Random   (randomRIO)

The problem
===========

Lorem ipsum generators usually create random but realistic-looking text by using
sample data, on which a model is trained. A very simple example of that is just
to pick words according to their frequencies. Let us take some sample data from
a song that fundamentally changed the music industry in the early 2000s:

<blockquote>
Badger badger badger</br>
Mushroom mushroom</br>
Badger badger badger</br>
Panic, a snake
Badger badger badger
Oh, it's a snake!</blockquote>

This gives us the following frequency list:

> badgers :: [(String, Int)]
> badgers =
>     [ ("a",        2)
>     , ("badger",   9)
>     , ("it's",     1)
>     , ("mushroom", 2)
>     , ("oh",       1)
>     , ("panic",    1)
>     , ("snake",    2)
>     ]

The sum of all the frequencies in this list is 11. This means that we will e.g.
pick "badger" with a chance of 6/11. We can naively implement this by expanding
the list so it contains the items in the given frequencies and then picking one.

> decodeRle :: [(a, Int)] -> [a]
> decodeRle []            = []
> decodeRle ((x, f) : xs) = replicate f x ++ decodeRle xs

> sample1 :: [(a, Int)] -> IO a
> sample1 freqs = do
>     let expanded = decodeRle freqs
>     idx <- randomRIO (0, length expanded - 1)
>     return $ expanded !! idx

This is obviously extremely inefficient, and it is not that hard to come up with
a better definition: we do not expand the list, and use a specialised indexing
function for frequency lists.

> indexFreqs :: Int -> [(a, Int)] -> a
> indexFreqs _   [] = error "please reboot computer"
> indexFreqs idx ((x, f) : xs)
>     | idx < f     = x
>     | otherwise   = indexFreqs (idx - f) xs

> sample2 :: [(a, Int)] -> IO a
> sample2 freqs = do
>     idx <- randomRIO (0, sum (map snd freqs) - 1)
>     return $ indexFreqs idx freqs

However, `sample2` is still relatively slow when we our sample data consists of
a large amount of text (imagine what happens if we have a few thousand different
words). Can we come up with a better but still elegant solution?

Note that lorem ipsum generators generally employ more complicated strategies
than just picking a word according to the frequencies in the sample data.
Usually, algorithms based on [Markov Chains] are used. But even when this is the
case, picking a word with some given frequencies is still a subproblem that
needs to be solved.

[Markov Chains]: http://en.wikipedia.org/wiki/Markov_chain

Frequency Trees
===============

It is easy to see why `sample2` is relatively slow: indexing in a linked list is
expensive. Purely functional programming languages usually solve this by using
trees instead of lists where fast indexing is required. We can use a similar
approach here.

A leaf in the tree simply holds an item and its frequency. A branch also holds
a frequency -- namely, the sum of the frequencies of its children. By storing
this computed value, we will be able to write a fast indexing this method.

> data FreqTree a
>     = Leaf   !Int !a
>     | Branch !Int (FreqTree a) (FreqTree a)
>     deriving (Show)

A quick utility function to get the sum of the frequencies in such a tree:

> sumFreqs :: FreqTree a -> Int
> sumFreqs (Leaf   f _)   = f
> sumFreqs (Branch f _ _) = f

Let us look at the tree for `badgers` (we will discuss how this tree is computer
later):

> badgersTree :: FreqTree String
> badgersTree =
>     (Branch 11
>         (Leaf 6 "badger")
>         (Branch 5
>             (Branch 3
>                 (Leaf 2 "mushroom")
>                 (Leaf 1 "snake"))
>             (Branch 2
>                 (Leaf 1 "a")
>                 (Leaf 1 "panic"))))

Once we have this structure, it is not hard to write a faster indexing function,
which is basically a search in a binary tree:

> indexFreqTree :: Int -> FreqTree a -> a
> indexFreqTree idx tree = case tree of
>     (Leaf _ x)             -> x
>     (Branch _ l r)
>         | idx < sumFreqs l -> indexFreqTree idx                l
>         | otherwise        -> indexFreqTree (idx - sumFreqs l) r

> sample3 :: FreqTree a -> IO a
> sample3 tree = do
>     idx <- randomRIO (0, sumFreqs tree - 1)
>     return $ indexFreqTree idx tree

> balancedTree :: Ord a => [(a, Int)] -> FreqTree a
> balancedTree =
>     go . filter ((> 0) . snd) . M.toList . M.fromListWith (+)
>   where
>     go []       = error "balancedTree: Empty list"
>     go [(x, f)] = Leaf f x
>     go xs       =
>         let half     = length xs `div` 2
>             (ys, zs) = splitAt half xs
>             f        = sum $ map snd xs
>         in Branch f (go ys) (go zs)

![A nicely balanced tree](/images/2013-11-20-badgers-balanced.png)

> -- | This function builds a 'FreqTree' that has good performance
> -- characteristics in most cases.
> partitionedTree :: Ord a => [(a, Int)] -> FreqTree a
> partitionedTree =
>     listToTree . sortBy (\(_, x) (_, y) -> compare y x) .
>     filter ((> 0) . snd) .
>     M.toList . M.fromListWith (+)

![A tree built using partitioning](/images/2013-11-20-badgers-partitioned.png)

> listToTree :: [(a, Int)] -> FreqTree a
> listToTree []       = error "frequencyMapToTree: Empty list"
> listToTree [(x, f)] = Leaf f x
> listToTree xs       =
>     let (l, lSum, r, rSum) = partition xs
>     in Branch (lSum + rSum)
>         (listToTree l) (listToTree r)

> partition :: [(a, Int)] -> ([(a, Int)], Int, [(a, Int)], Int)
> partition = go [] 0 [] 0
>   where
>     go l lSum r rSum ls = case ls of
>         []                 -> (reverse l, lSum, reverse r, rSum)
>         ((x, f) : xs)
>             | lSum <= rSum -> go ((x, f) : l) (f + lSum) r rSum xs
>             | otherwise    -> go l lSum ((x, f) : r) (f + rSum) xs

> harmonicNumber :: Int -> Double
> harmonicNumber n = sum [1 / fromIntegral k | k <- [1 .. n]]

> probabilities :: Double -> Int -> [Double]
> probabilities s n =
>     [1 / (fromIntegral k ** s * hn) | k <- [1 .. n]]
>   where
>     hn = harmonicNumber n

> averageDepthPartitioned :: Double -> Int -> Double
> averageDepthPartitioned s n = sum
>     [ log invProb / invProb
>     | k <- [1 .. n]
>     , let invProb = fromIntegral k ** s * hn
>     ]
>   where
>     hn = harmonicNumber n

> averageDepthBalanced :: Int -> Double
> averageDepthBalanced = log . fromIntegral

> writeData :: IO ()
> writeData = writeFile "data" $ unlines $
>     [ show n ++ "," ++
>         show (averageDepthBalanced n) ++ "," ++
>         show (averageDepthPartitioned 1 n)
>     | n <- [1000, 2000 .. 100000]
>     ]

> graphviz :: String -> FreqTree String -> IO ()
> graphviz fileName ft = writeFile fileName $ unlines $
>     ["graph freqtree {"]  ++
>     ["node [shape=box];"] ++
>     go "t" ft             ++
>     ["}"]
>   where
>     go n (Leaf   f x)   =
>         [n ++ " [label=\"" ++ x ++ ", " ++ show f ++ "\"];"]
>     go n (Branch f l r) =
>         go (n ++ "l") l                                          ++
>         go (n ++ "r") r                                          ++
>         [n ++ " [label=\"" ++ show f ++ "\", shape=plaintext];"] ++
>         [n ++ " -- " ++ n ++ "l;"]                               ++
>         [n ++ " -- " ++ n ++ "r;"]

