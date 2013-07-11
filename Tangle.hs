module Tangle (mangle) where

import System.Random (RandomGen, randomR)
import Data.List (tails)
import qualified Data.Map as M

type Model a = M.Map [a] (M.Map a Int)

-- | Generate a novel text based on an input text by resolving the input
-- into a Markov model with transition memory at most n and following the chain.
mangle :: (Ord a, RandomGen g) => Int -> [a] -> g -> [a]
mangle n items = maybe [] go . initialState
  where
    go = uncurry $ chain (model n items)
    initialState = sample (n `grams` items)

-- | Build an order n model from a sequence of states.
model :: Ord a => Int -> [a] -> Model a
model n items = train M.empty ngrams
  where
    train = foldl $ \m l -> transition m (init l) (last l)
    ngrams = concatMap (flip grams items) [2..n]

chain :: (Ord a, RandomGen g) => Model a -> [a] -> g -> [a]
chain mdl st = maybe [] next . sample candidates
  where
    next (n,r) = n : chain mdl (tail st ++ [n]) r
    candidates = expand (successors mdl st)
    expand = concatMap $ \(a,b) -> replicate b a

-- | Train a model with a state transition.
transition :: Ord a => Model a -> [a] -> a -> Model a
transition mdl st ev = M.insert st newCounts mdl
  where
    newCounts = M.insert ev newCount oldCounts
    newCount  = succ $ M.findWithDefault 0 ev oldCounts
    oldCounts = M.findWithDefault M.empty st mdl

-- | All length-n sublists.
grams :: Int -> [a] -> [[a]]
grams n = filter ((==n) . length) . map (take n) . tails

-- | Return the weighted options for the next output from a state.
successors :: Ord a => Model a -> [a] -> [(a, Int)]
successors mdl = concatMap counts . tails
  where
    counts i = maybe [] M.toList (M.lookup i mdl)

-- | Sample a random element from a list.
sample :: RandomGen g => [a] -> g -> Maybe (a,g)
sample [] = const Nothing
sample l  = return . (\(i, r) -> (l !! i, r)) . randomR (0, length l - 1)

