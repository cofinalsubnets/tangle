module Tangle (mangle) where

import Model
import System.Random (RandomGen)

-- | Generate a novel text based on an input text by resolving the input
-- into a Markov model with transition memory at most n and following the chain.
mangle :: (Ord a, RandomGen g) => Int -> [a] -> g -> [a]
mangle n items = maybe [] go . initialState
  where
    go = uncurry $ chain (model n items)
    initialState = sample (n `grams` items)

