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

> import           Data.List       (foldl', sortBy)
> import           Data.Map.Strict (Map)
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
Panic, a snake</blockquote>

This gives us the following frequency list:

> badgers :: [(String, Int)]
> badgers =
>     [ ("badger",   6)
>     , ("mushroom", 2)
>     , ("panic",    1)
>     , ("a",        1)
>     , ("snake",    1)
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

> data FrequencyTree a
>     = FrequencyTreeLeaf {-# UNPACK #-} !Int !a
>     | FrequencyTreeNode {-# UNPACK #-} !Int (FrequencyTree a) (FrequencyTree a)
>     deriving (Read)


> instance Show a => Show (FrequencyTree a) where
>     show = unlines . show'
>       where
>         indent             = map ("    " ++)
>         mapLast _ []       = []
>         mapLast f [x]      = [f x]
>         mapLast f (x : xs) = x : mapLast f xs

>         show' (FrequencyTreeLeaf f i) =
>             ["(FrequencyTreeLeaf " ++ show f ++ " " ++ show i ++ ")"]
>         show' (FrequencyTreeNode f l r) =
>             ["(FrequencyTreeNode " ++ show f] ++
>             indent (show' l) ++
>             indent (mapLast (++ ")") $ show' r)


> singleton :: Ord a => a -> FrequencyTree a
> singleton = FrequencyTreeLeaf 1


> fromList :: Ord a => [a] -> FrequencyTree a
> fromList = frequencyMapToTree . listToFrequencyMap


> fromFrequencies :: Ord a => [(a, Int)] -> FrequencyTree a
> fromFrequencies = frequencyMapToTree . M.fromListWith (+)


> append :: Ord a
>        => FrequencyTree a -> FrequencyTree a -> FrequencyTree a
> append x y = FrequencyTreeNode (treeSum x + treeSum y) x y


> optimize :: Ord a => FrequencyTree a -> FrequencyTree a
> optimize = frequencyMapToTree . frequencyTreeToMap


> treeSum :: Ord a => FrequencyTree a -> Int
> treeSum (FrequencyTreeLeaf f _)   = f
> treeSum (FrequencyTreeNode f _ _) = f


> treeSample :: Ord a => Int -> FrequencyTree a -> a
> treeSample seed initial =
>     go (seed `mod` treeSum initial) initial
>   where
>     go _   (FrequencyTreeLeaf _ i)   = i
>     go idx (FrequencyTreeNode _ l r)
>         | idx < lsum                 = go idx          l
>         | otherwise                  = go (idx - lsum) r
>       where
>         lsum = treeSum l


> treeSampleIO :: Ord a => FrequencyTree a -> IO a
> treeSampleIO ft = do
>     idx <- randomRIO (0, treeSum ft - 1)
>     return $ treeSample idx ft


> type FrequencyMap a = Map a Int


> frequencyTreeToMap :: Ord a => FrequencyTree a -> FrequencyMap a
> frequencyTreeToMap (FrequencyTreeLeaf f i)   = M.singleton i f
> frequencyTreeToMap (FrequencyTreeNode _ l r) =
>     M.unionWith (+) (frequencyTreeToMap l) (frequencyTreeToMap r)


> -- | This function builds a 'FrequencyTree' that has good performance
> -- characteristics in most cases.
> frequencyMapToTree :: Ord a => FrequencyMap a -> FrequencyTree a
> frequencyMapToTree =
>     listToTree . sortBy (\(_, x) (_, y) -> compare y x) .
>     filter ((> 0) . snd) . M.toList

> listToTree :: [(a, Int)] -> FrequencyTree a
> listToTree []       = error "frequencyMapToTree: Empty FrequencyMap"
> listToTree [(x, f)] = FrequencyTreeLeaf f x
> listToTree xs       =
>     let (l, lSum, r, rSum) = partition xs
>     in FrequencyTreeNode (lSum + rSum)
>         (listToTree l) (listToTree r)

> partition :: [(a, Int)] -> ([(a, Int)], Int, [(a, Int)], Int)
> partition = go [] 0 [] 0
>   where
>     go l lSum r rSum ls = case ls of
>         []                 -> (reverse l, lSum, reverse r, rSum)
>         ((x, f) : xs)
>             | lSum <= rSum -> go ((x, f) : l) (f + lSum) r rSum xs
>             | otherwise    -> go l lSum ((x, f) : r) (f + rSum) xs


> listToFrequencyMap :: Ord a => [a] -> FrequencyMap a
> listToFrequencyMap = foldl' (\hm i -> M.insertWith (+) i 1 hm) M.empty
