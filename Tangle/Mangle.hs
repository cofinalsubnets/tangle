module Tangle.Mangle
( mangle
) where

import Data.List

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n ls = s : (chunks n l)
  where (s, l) = splitAt n ls

synthesize :: Eq a => Int -> [[a]] -> [[a]]
synthesize _ [] = []
synthesize n (base:strs) = case synth base strs of
  (Nothing, _)  -> synthesize n strs
  (Just m, rem) -> m:(synthesize n rem)

  where synth base strs = case ms of []     -> (Nothing, rem)
                                     (w:ws) -> (Just $ prel ++ w, ws ++ rem)

          where (ms, rem)    = partition (isPrefixOf term) strs
                (prel, term) = splitAt (length base - n) base

mangle :: Int -> Int -> String -> String
mangle chunk merge = unwords . map unwords . synthesize merge . chunks chunk . words

main = getContents >>= putStrLn . mangle 4 1

