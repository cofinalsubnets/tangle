module Tangle.Mangle ( mangle) where

import Data.List

slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice n ls = s : (slice n l)
  where (s, l) = splitAt n ls

synthesize :: Eq a => Int -> [[a]] -> [[a]]
synthesize _ [] = []
synthesize n (base:strs) = case synth base strs n of
  (Nothing, _)  -> synthesize n strs
  (Just m, rem) -> m:(synthesize n rem)

synth :: Eq a => [a] -> [[a]] -> Int -> (Maybe [a], [[a]])
synth base strs th = case ms of []     -> (Nothing, rem)
                                (w:ws) -> (Just $ prel ++ w, rem ++ ws)
  where (ms, rem)    = partition ((term==) . take th) strs
        (prel, term) = splitAt (length base - th) base

mangle :: Int -> Int -> String -> String
mangle m n = unwords . map unwords . synthesize m . slice n . words

