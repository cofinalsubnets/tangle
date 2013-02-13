module Tangle.Mangle
( mangle
, mangleExamples
) where

import Data.List

mangle   :: Int -> String -> String
mangle n = unwords . snth . slices n . words

slices :: Int -> [a] -> [[a]]
slices _ [] = []
slices n ls = s : (slices n l)
  where (s, l) = splitAt n ls

lctx     :: Eq a => [a] -> [a] -> Int
lctx x y = if lx > ly then lctx' (drop (lx - ly) x) y else lctx' x y
  where (lx,ly) = (length x, length y)
        lctx' x y
          | null x         = 0
          | isPrefixOf x y = length x
          | otherwise      = lctx' (tail x) y

bestMerge       :: [String] -> [[String]] -> (Maybe ([String],Int), [[String]])
bestMerge mh cs = foldl mcheck (Nothing, []) cs
  where mcheck (m,l) c = case lctx mh c of 0 -> (m,c:l)
                                           n -> case m of Nothing      -> (Just (c,n), l)
                                                          Just (c',n') -> if n > n'
                                                                          then (Just (c,n), c':l)
                                                                          else (m, c:l)

snth        :: [[String]] -> [String]
snth []     = []
snth (c:cs) = let (ms,_,_) = until (\(_,_,l) -> null l) snth' ([c],c,cs) in concat $ reverse ms

snth' :: ([[String]], [String], [[String]]) -> ([[String]], [String], [[String]])
snth' (ms, mh, cs) = let (m,nxt) = bestMerge mh cs
                     in case m of Nothing    -> let nh = head nxt in (nh:ms, nh, tail nxt)
                                  Just (c,n) -> ((drop n c):ms, c, nxt)

mangleExamples = [ (bestMerge mergeHead chunks  == (Just (c2, 2), [c1]),   "bestMerge success case")
                 , (bestMerge mergeHead chunks' == (Nothing, [c2',c1']),   "bestMerge failure case")
                 ]

  where mergeHead = words $ "oh hello there friend"
        chunks    = [c1,c2]
        c1        = words $ "friend hello oh wow"
        c2        = words $ "there friend no way"
        chunks'   = [c1',c2']
        c1'       = words $ "a b c d"
        c2'       = words $ "e f g h"

