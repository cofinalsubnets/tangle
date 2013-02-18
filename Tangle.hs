module Tangle (mangle) where

import System.Random
import Data.Maybe
import qualified Trie.Count as TC
import qualified Trie.Map as TM

type Counter = TC.CountTrie Char
type WordMap = TM.TrieMap String Counter

-- | Generate a new text based on the input text by resolving the input into a
-- Markov model (where states are words) and following the chain.
mangle :: Int -> String -> [String] -> StdGen -> [String]
mangle n = wStream . train n

-- | Rotate lst to the left by n places.
rot :: Int -> [a] -> [a]
rot n lst = t ++ h
  where (h,t) = splitAt n' lst
        n'    = n `mod` length lst

-- | Split a list into non-overlapping sublists of size n (except possibly for
-- the last sublist)
slices :: Int -> [a] -> [[a]]
slices _ [] = []
slices n ls = s : (slices n l)
  where (s, l) = splitAt n ls

train :: Int -> String -> WordMap
train n str = foldl count TM.mkTrie groups
  where groups = concat [groupWs (words str) i | i <- [2..n]]
        count d ws = TM.insert (init ws) (TC.insert (last ws) d') d
          where d' = fromMaybe TC.mkTrie $ TM.lookup (init ws) d

rots :: [a] -> Int -> [[a]]
rots ws n = [rot i ws | i <- [0..n-1]]

groupWs :: [a] -> Int -> [[a]]
groupWs ws n = filter (\s -> length s == n) $ concatMap (slices n) $ rots ws n
        
nxts :: [String] -> WordMap -> [(String,Int)]
nxts [] _ = []
nxts ws d = (TC.toList . fromMaybe TC.mkTrie . TM.lookup ws $ d) ++ nxts (tail ws) d

select :: [(String,Int)] -> StdGen -> Maybe (String,StdGen)
select []   _   = Nothing
select opts rng = Just (snd $ foldl1 sel opts', rng')
  where wts = scanl1 (+) $ map snd opts
        opts' = wts `zip` map fst opts
        sel s s' = if fst s' > n then s else s'
        (n, rng') = randomR (1,last wts) rng

wStream :: WordMap -> [String] -> StdGen -> [String]
wStream dict state rng = case select (nxts state dict) rng of
  Nothing -> []
  Just (nxt,rng') -> nxt:(wStream dict state' rng')
    where state' = (init state) ++ [nxt]

