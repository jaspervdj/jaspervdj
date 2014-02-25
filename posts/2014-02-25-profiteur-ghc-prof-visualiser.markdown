---
title: Profiteur: a visualiser for Haskell GHC .prof files
description: A nicer way to browse big profile files
tags: haskell
---

# Introduction

[GHC] comes with some [amazing tools] to do profiling of Haskell programs. In
`.prof` files, you can see exactly in which function most time is spent and
where most allocation is done.

[GHC]: http://www.haskell.org/ghc/
[amazing tools]: http://book.realworldhaskell.org/read/profiling-and-optimization.html

However, at [Erudify], we have a big amount of Haskell code -- and at this point
`.prof` files can become very unwieldy, and the text representation is harder to
grok.

[Erudify]: http://www.erudify.com/

This is why I coded [profiteur], a simple HTML-based visualiser for GHC `.prof`
files.

[profiteur]: http://github.com/jaspervdj/profiteur

# Installation

Installation is easy:

    $ cabal install profiteur

# Usage

Let us grab a [sample program] from the [HaskellWiki]. The code of this sample
program can be found in the appendix. I saved this file as `binary-trees.hs`.

[sample program]: http://www.haskell.org/haskellwiki/Shootout/Binary_trees
[HaskellWiki]: http://www.haskell.org/haskellwiki/Haskell

First, we compile it with profiling enabled:

    $ ghc --make -auto-all -prof -rtsopts binary-trees.hs
    [1 of 1] Compiling Main             ( binary-trees.hs, binary-trees.o )
    Linking binary-trees ...

We run it in profiling mode:

    $ ./binary-trees 10 +RTS -p -RTS
    stretch tree of depth 11     check: -1
    2048     trees of depth 4    check: -2048
    512  trees of depth 6    check: -512
    128  trees of depth 8    check: -128
    32   trees of depth 10   check: -32
    long lived tree of depth 10  check: -1

This generates the file `binary-trees.prof`. We can pass that to `profiteur`:

    $ profiteur binary-trees.prof
    Wrote binary-trees.prof.html

Open the resulting file in your (modern) favorite browser and you are good to
go! [Here is the resulting HTML file] so you can have a look without installing
`profiteur`.

[Here is the resulting HTML file]: /files/binary-trees.prof.html

As always, patches and pull requests are welcome on [GitHub].

[GitHub]: https://github.com/jaspervdj/profiteur

# Appendix

Code used:

~~~~~{.haskell}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
--
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- Contributed by Don Stewart
--
 
import System.Environment
import Data.Bits
import Text.Printf
 
data Tree = Nil | Node !Int Tree Tree
 
minN = 4
 
io s !n !t = printf "%s of depth %d\t check: %d\n" s n t
 
main = do
    n <- getArgs >>= readIO . head
    let maxN     = max (minN + 2) n
        stretchN = maxN + 1
 
    -- stretch memory tree
    let c = check (make 0 stretchN)
    io "stretch tree" stretchN c
 
    -- allocate a long lived tree
    let long    = make 0 maxN
 
    -- allocate, walk, and deallocate many bottom-up binary trees
    let vs = depth minN maxN
    mapM_ (\((m,d,i)) -> io (show m ++ "\t trees") d i) vs
 
    -- confirm the the long-lived binary tree still exists
    io "long lived tree" maxN (check long)
 
-- generate many trees
depth :: Int -> Int -> [(Int,Int,Int)]
depth !d !m
    | d <= m    = (2*n,d,sumT d n 0) : depth (d+2) m
    | otherwise = []
  where !n = 1 `shiftL` (m - d + minN)
 
-- allocate and check lots of trees
sumT :: Int -> Int -> Int -> Int
sumT !d 0 t = t
sumT  d i t = sumT d (i-1) (t + a + b)
  where a = check (make i    d)
        b = check (make (-i) d)
 
-- traverse the tree, counting up the nodes
check :: Tree -> Int
check Nil          = 0
check (Node i l r) = i + check l - check r
 
-- build a tree
make :: Int -> Int -> Tree
make i 0 = Node i Nil Nil
make i d = Node i (make (i2-1) d2) (make i2 d2)
  where i2 = 2*i; d2 = d-1
~~~~~
