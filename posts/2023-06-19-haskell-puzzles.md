---
title: Haskell Puzzles
description: Some Haskell Puzzles from ZuriHac 2023
tags: haskell
---

At [ZuriHac 2023](https://zfoh.ch/zurihac2023), I worked on some Haskell Puzzles
together with
[Alex](https://www.asayers.com/),
[Francesco](https://mazzo.li/),
[Jussi](https://github.com/joamaki) and
[Patrick](https://github.com/chpatrick).

We came up with some puzzles where you are given some Haskell tokens, and the
goal is to **rearrange them into an expression that evaluates to a desired
value**.


Here a simple warmup exercise:

<div id="puzzle_iterate" class="puzzle" data-puzzle='{
    "goal": "[0,1,2,3,4]",
    "tokens": [
        {"text": "iterate", "x": 70,  "y": 40,  "hint": "(a → a) → a → [a]"},
        {"text": "0",       "x": 150, "y": 100                             },
        {"text": "succ",    "x": 100, "y": 100, "hint": "n → n"            },
        {"text": "take",    "x": 180, "y": 40,  "hint": "Int → [a] → [a]"  },
        {"text": "5",       "x": 70,  "y": 160                             },
        {"text": "(",       "x": 260, "y": 40                              },
        {"text": ")",       "x": 30,  "y": 160                             }
    ]
}'></div>

This is a bit more of a puzzle.  What Monad are we looking for?

<div id="puzzle_join" class="puzzle" data-puzzle='{
    "goal": "32",
    "tokens": [
        {"text": "5",       "x": 50,  "y": 80                              },
        {"text": "1",       "x": 30,  "y": 140                             },
        {"text": "(+)",     "x": 200, "y": 150                             },
        {"text": "(",       "x": 120, "y": 160                             },
        {"text": ")",       "x": 240, "y": 30                              },
        {"text": "iterate", "x": 120, "y": 30,  "hint": "(a → a) → a → [a]"},
        {"text": "join",    "x": 150, "y": 100, "hint": "m (m a) → m a"    },
        {"text": "!!",      "x": 240, "y": 100, "hint": "[a] → Int → a"    }
    ]
}'></div>

How is _e_ defined again?

<div id="puzzle_e" class="puzzle" data-puzzle='{
    "goal": "2.7182818284590455",
    "tokens": [
        {"text": "sum",      "x": 260, "y": 140, "hint": "[n] → n"                },
        {"text": "(/)",      "x": 150, "y": 150                                   },
        {"text": "scanl1",   "x": 100, "y": 70,  "hint": "(a → a → a) → [a] → [a]"},
        {"text": "succ",     "x": 50,  "y": 160, "hint": "n → n"                  },
        {"text": "$",        "x": 90,  "y": 160                                   },
        {"text": "[1..100]", "x": 250, "y": 50                                    },
        {"text": "$",        "x": 200, "y": 150                                   }
    ]
}'></div>


The next puzzle is a bit tricky.  Is there a 2 missing?

<div id="puzzle_let" class="puzzle" data-puzzle='{
    "goal": "8",
    "tokens": [
        {"text": "2 + 2", "x": 60,  "y": 40 },
        {"text": "*",     "x": 110, "y": 40 },
        {"text": "2",     "x": 140, "y": 40 },
        {"text": "in",    "x": 200, "y": 70 },
        {"text": "a",     "x": 235, "y": 70 },
        {"text": "+",     "x": 270, "y": 70 },
        {"text": "+",     "x": 200, "y": 120},
        {"text": "=",     "x": 235, "y": 120},
        {"text": "b",     "x": 270, "y": 120},
        {"text": "let",   "x": 200, "y": 170},
        {"text": "a",     "x": 235, "y": 170},
        {"text": "b",     "x": 270, "y": 170}
    ]
}'></div>

Good job making it this far!  Here is a final puzzle.

<div id="puzzle_fin" class="puzzle" data-puzzle='{
    "goal": "\"fin\"",
    "tokens": [
        {"text": "0",    "x": 75,  "y": 30 },
        {"text": "1",    "x": 125, "y": 30 },
        {"text": "2",    "x": 175, "y": 30 },
        {"text": "3",    "x": 225, "y": 30 },
        {"text": "$",    "x": 50,  "y": 150},
        {"text": "$",    "x": 40,  "y": 100},
        {"text": "$",    "x": 80,  "y": 110},
        {"text": "take", "x": 200, "y": 110},
        {"text": "drop", "x": 260, "y": 90 },
        {"text": "show", "x": 180, "y": 160},
        {"text": "/",    "x": 250, "y": 160}
    ]
}'></div>

Fin!

---

Haskell evaluation powered by [tryhaskell.org](https://tryhaskell.org).
UI powered by [some messy JavaScript](/files/2023-06-19-haskell-puzzles.js).

We playtested this with simple pieces of paper during the event and at the final
presentation we played a multiplayer version where each player controls only one
token.

We actually found the single player to be more fun, and since we already had
some client code I decided to clean it up a bit and make a single player version
available here.

Thanks for playing!

<script type="text/JavaScript" src="/files/2023-06-19-haskell-puzzles.js"></script>
