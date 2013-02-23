module Tangle (mangle) where

import System.Random
import Data.Array
import Data.Maybe
import qualified Trie.Count as TC
import qualified Trie.Map as TM

type Counter = TC.CountTrie Char
type WordMap = TM.TrieMap String Counter

-- | Generate a new text based on an input text and starting with a
-- seed text, by resolving the input into a Markov model (where states
-- are words) and following the chain.
-- TODO Break it into words at this level, and make the rest of the
-- code agnostic about alphabet?
mangle :: Int -> String -> [String] -> StdGen -> [String]
mangle n = wStream . train n

-- | Return a word n-gram Markov model for str.
train :: Int -> String -> WordMap
train n str = foldl count TM.mkTrie groups
  where groups = concat [sublists i (words str) | i <- [2..n+1]]
        count d ws = TM.insert (init ws) (TC.insert (last ws) d') d
          where d' = fromMaybe TC.mkTrie $ TM.lookup (init ws) d

-- | Return the length-k sublists of xs, in order.
sublists :: Int -> [a] -> [[a]]
sublists k xs = [map (a!) [i..i+k-1] | i <- [0..n-k]]
  where a = listArray (0, n-1) xs
        n = length xs

-- | Generate a random sequence from a model and a starting state.
wStream :: WordMap -> [String] -> StdGen -> [String]
wStream dict state rng = case select (nxts state dict) rng of
  Nothing -> []
  Just (nxt,rng') -> nxt:(wStream dict state' rng')
    where state' = (init state) ++ [nxt]

-- | Return the weighted options for the next output from a state.
nxts :: [String] -> WordMap -> [(String,Int)]
nxts [] _ = []
nxts ws d = (TC.toList . fromMaybe TC.mkTrie . TM.lookup ws $ d) ++ nxts (tail ws) d

-- | Sample from weighted options with probability proportional to weight.
-- String could be a type variable instead, right?
-- Could Int be more generic too?
select :: [(String,Int)] -> StdGen -> Maybe (String,StdGen)
select []   _   = Nothing
select opts rng = Just (snd $ foldl1 sel opts', rng')
  where wts = scanl1 (+) $ map snd opts
        opts' = wts `zip` map fst opts
        sel s s' = if fst s' > n then s else s'
        (n, rng') = randomR (1,last wts) rng
