module Tangle (mangle) where

import System.Random
import Data.Maybe
import qualified Trie.Count as TC
import qualified Trie.Map as TM

type Counter = TC.CountTrie Char
type WordMap = TM.TrieMap Char Counter

-- | Generate a new text based on the input text by resolving the input into a
-- Markov model (where states are words) and following the chain.
mangle :: String -> String -> StdGen -> [String]
mangle = wStream . train TM.mkTrie

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

train :: WordMap -> String -> WordMap
train dict txt = train' (words txt)
  where train' ws = foldl count dict $ prep ws 
        prep ws = slices 2 ws ++ (slices 2 $ rot 1 ws)
        count d [w1,w2] = TM.insert w1 (TC.insert w2 d') d
          where d' = fromMaybe TC.mkTrie $ TM.lookup w1 d
        count d _ = d
        
nxts :: String -> WordMap -> [(String,Int)]
nxts w = TC.toList . fromMaybe TC.mkTrie . TM.lookup w

select :: [(String,Int)] -> StdGen -> Maybe (String,StdGen)
select []   _   = Nothing
select opts rng = Just (snd $ foldl1 sel opts', rng')
  where wts = scanl1 (+) $ map snd opts
        opts' = wts `zip` map fst opts
        sel s s' = if fst s' > n then s else s'
        (n, rng') = randomR (1,last wts) rng

wStream :: WordMap -> String -> StdGen -> [String]
wStream dict state rng = case select (nxts state dict) rng of
  Nothing -> []
  Just (state',rng') -> state':(wStream dict state' rng')

