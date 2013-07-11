module Tangle (mangle, mangleText) where

import Data.Map (Map)
import System.Random (RandomGen, randomR)
import Data.List (tails)
import qualified Data.Map as M

type Model a = Map [a] (Map a Int)

-- | Generate a novel text based on an input text by resolving the input
-- into a Markov model with transition memory at most n and following the chain.
mangle :: (Ord a, RandomGen g) => Int -> [a] -> g -> [a]
mangle n ws = maybe [] start . initialState
  where
    start = uncurry $ chain (model n ws)
    initialState = sample $ n `grams` ws

mangleText :: RandomGen g => Int -> String -> g -> [String]
mangleText = (. words) . mangle

-- | Build an order i model from a sequence of states.
model :: Ord a => Int -> [a] -> Model a
model i ws = train M.empty ngrams
  where
    train = foldl $ \m l -> transition m (init l) (last l)
    ngrams = concat [n `grams` ws | n <- [2..i]]

chain :: (Ord a, RandomGen g) => Model a -> [a] -> g -> [a]
chain m st = maybe [] continue . sample candidates
  where
    continue (n, rng) = n : chain m (tail st ++ [n]) rng
    candidates = expand (successors m st)
    expand = concatMap (uncurry $ flip replicate)

-- | Train a model with a state transition.
transition :: Ord a => Model a -> [a] -> a -> Model a
transition m s e = M.insert s (M.insert e count counts) m
  where
    count  = succ $ M.findWithDefault 0 e counts
    counts = M.findWithDefault M.empty s m

-- | All length-n sublists.
grams :: Int -> [a] -> [[a]]
grams n = filter ((==n) . length) . map (take n) . tails

-- | Return the weighted options for the next output from a state.
successors :: Ord a => Model a -> [a] -> [(a, Int)]
successors d = concatMap (maybe [] M.toList . flip M.lookup d) . tails

-- | Sample a random element from a list.
sample :: RandomGen g => [a] -> g -> Maybe (a,g)
sample [] = const Nothing
sample l  = return . (\(i, r) -> (l !! i, r)) . randomR (0, length l - 1)

