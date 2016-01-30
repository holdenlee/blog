---
title: A Post With Math
subtitle: Can I render math?
tags: math
published: 2015-01-02
---

See my [first post](2015-01-01-hello-world.html).

This is math:

$$ \int_1^x \frac{1}{u}\,du = \ln x.$$

More math:

$$
\pi = 4\sum_{n=0}^{\infty} (-1)^n \fc{1}{2n+1}
$$

Inline: $a^p\equiv a \pmod{p}$.[^f1]

Macro: $\fc{1}{2}$

This is a reference:

Text: Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

Haskell code:

```haskell
{-# OPTIONS
    -XPatternSynonyms
    -XTemplateHaskell
#-}

module CYK where

import Control.Lens
import Control.Monad
import Control.Monad.Free
--import Control.Monad.Trans.RWS.Lazy
import Control.Monad.Trans.State.Lazy
--import Control.Monad.Trans.Writer.Lazy
import qualified Data.Array as A
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import qualified Data.MultiMap as MM
--import Data.Tree as T
import Utilities
--https://github.com/holdenlee/haskell-utilities

--type WS w s = RWST () w s

{-| Array helpers -}
fillArray :: (A.Ix i) => (i,i) -> e -> A.Array i e
fillArray (lo,hi) =
   A.listArray (lo, hi) . repeat

instance (Show k, Show a) => Show (MM.MultiMap k a) where
  show = show . MM.toMap

{-
aset :: (Ix i) => i -> e -> Array i e -> Array i e
aset i e = (//[(i,e)])-}

{-| Data types -}

type Symbol = Either

pattern Nonterminal a = Left a 
pattern Terminal t = Right t

--type Grammar a = M.Map a [Symbol a]
data CNFGrammar a t = CNFGrammar {_unitProds :: [(a, t)],
                                  _prods :: [(a, [a])],
                                  _charMap :: MM.MultiMap t Int} deriving Show

makeLenses ''CNFGrammar

initCNF :: (Ord t) => [(a, t)] -> [(a, [a])] -> CNFGrammar a t
initCNF p1 p2 =
  CNFGrammar {_unitProds = p1,
              _prods = p2,
              _charMap = for' (zenumerate p1) MM.empty (\(i,(_,r)) -> MM.insert r i)}

type ProdTable a t = A.Array (Int, Int) (MM.MultiMap a (Either (Int, [a]) t))

{-| CYK parse -}
cykParse' :: (Ord a, Ord t) => [t] -> CNFGrammar a t -> State (ProdTable a t) Bool
cykParse' str grammar = do
  let n = (length str)
  put (fillArray ((1, 0),(n, n - 1)) MM.empty)
  --for all characters in the string [t]
  forM_ [0..(n-1)] (\i -> 
    --for each unit production rule r -> t
    forM_ (MM.lookup (str!!i) (grammar ^. charMap)) (\j ->
      let (r,t) = (grammar ^. unitProds)!!j
      in modify (ix (1,i) %~ MM.insert r (Terminal t))                                             {-
    forM_ (grammar ^. unitProds) (\(r, t) -> 
      --set P[1,i,r] to point to the character
      if t == str!!i
         then modify (ix (1, i) %~ MM.insert r (Terminal t))
         else return ()-}
      )
    )
  --for each possible length of span
  forM_ [2..n] (\i ->
    --for each possible start of span
    forM_ [0..(n-i)] (\j ->
      --for each possible partition of span
      forM_ [1..(i-1)] (\k ->
        --for each production rule
        forM_ (grammar ^. prods) (\(a,[b,c]) ->
          modify (\p -> if MM.member (p A.! (k,j)) b && MM.member (p A.! (i-k,j+k)) c
                        then p & ix (i,j) %~ MM.insert a (Nonterminal (k,[b,c]))
                        else p)
          )
        )
      )
    )
  p <- get
  return (not $ MM.null (p A.! (n,0)))
  
gr = initCNF     [("VP","eats"),
                  ("NP","she"),
                  ("V","eats"),
                  ("P","with"),
                  ("N","artichoke"),
                  ("N","fork"),
                  ("Det","an"),
                  ("Det","a")]
                 [("S", ["NP","VP"]),
                  ("VP", ["VP","PP"]),
                  ("VP", ["V","NP"]),
                  ("PP", ["P","NP"]),
                  ("NP", ["Det","N"]),
--let's add some ambiguity
                  ("NP", ["NP","PP"])]

sent = words "she eats an artichoke with a fork"

parsed = runState (cykParse' sent gr) undefined

data LabeledList a t = LabeledList a [t] deriving Show

type LabTree a = Free (LabeledList a)

pattern LabTree a t = Free (LabeledList a t)

{-| Given the start, the length, and the type -}
parsings' :: (Ord a) => Int -> Int -> a -> ProdTable a t -> [LabTree a t]
parsings' len i a pt = do
  st <- MM.lookup a (fromMaybe MM.empty (pt ^? ix (len, i)))
  --format is Either (Int, [a]) t
  case st of
   Nonterminal (k,[l,r]) ->
     map (LabTree a) $ (\x y -> [x,y]) <$> parsings' k i l pt <*> parsings' (len-k) (i+k) r pt
   Terminal term ->
     [LabTree a [Pure term]]

parsings :: (Ord a) => ProdTable a t -> [LabTree a t]
parsings pt =
  let
    len = fst $ snd (A.bounds pt)
  in
   --for all the possible symbols...
   foldMap (\x -> parsings' len 0 x pt) (MM.keys (pt A.! (len, 0)))
     
ans = parsings $ execState (cykParse' sent gr) undefined
```

[^f1]: This is a footnote.
