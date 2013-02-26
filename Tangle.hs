module Tangle (mangle) where

import Data.Map (Map)
import System.Random
import Data.Array
import qualified Data.Map as M
import Data.Maybe
import Control.Applicative


-- | Generate a new text based on an input text and starting with a
-- seed text, by resolving the input into a Markov model (where states
-- are words) and following the chain.
mangle :: Int -> String -> StdGen -> [String]
mangle n str  = chain model seed
  where model = mkModel n ws :: Model String Int
        seed  = take n ws
        ws    = words str

type Model a b = Map [a] (Map a b)

mkModel :: (Ord a, Num b, Enum b) => Int -> [a] -> Model a b
mkModel n items = train n items $ M.fromList []

train :: (Ord a, Num b, Enum b) => Int -> [a] -> Model a b -> Model a b
train n items model = foldl observe' model subs
  where subs = concat [sublists i items | i <- [2..n+1]]
        observe' mdl es = let (ks,ev) = (,) <$> init <*> last $ es
                          in observe ks ev mdl

-- | Record a state transition, updating the model.
observe :: (Ord a, Num b, Enum b) => [a] -> a -> Model a b -> Model a b
observe ks ev model = M.insert ks updated model
  where updated = M.insert ev count counts
        count   = succ $ fromMaybe 0 $ M.lookup ev counts
        counts  = fromMaybe mkCounter $ M.lookup ks model

chain :: (Ord a, Ord b, Num b, Random b, RandomGen t) => Model a b -> [a] -> t -> [a]
chain model state rng = case select (nxts state model) rng of
  Nothing -> []
  Just (nxt,rng') -> nxt:(chain model (tail state ++ [nxt]) rng')

-- | Return an empty event counter for weighting state transitions.
mkCounter :: (Ord a, Num b) => Map a b
mkCounter = M.fromList []

-- | Return the length-k sublists of xs, in order.
sublists :: Int -> [a] -> [[a]]
sublists k xs = [map (a!) [i..i+k-1] | i <- [0..n-k]]
  where a = listArray (0, n-1) xs
        n = length xs

-- | Return the weighted options for the next output from a state.
nxts :: (Ord a, Num b) => [a] -> Model a b -> [(a,b)]
nxts [] _ = []
nxts ws d = (M.toList . fromMaybe mkCounter . M.lookup ws $ d) ++ nxts (tail ws) d

-- | Sample from weighted options with probability proportional to weight.
-- String could be a type variable instead, right?
-- Could Int be more generic too?
select :: (Ord a, Num b, Random b, Ord b) => RandomGen t => [(a,b)] -> t -> Maybe (a,t)
select []   _   = Nothing
select opts rng = Just (snd $ foldl1 sel opts', rng')
  where wts = scanl1 (+) $ map snd opts
        opts' = wts `zip` map fst opts
        sel s s' = if fst s' > n then s else s'
        (n, rng') = randomR (1,last wts) rng

