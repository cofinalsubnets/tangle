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
mangle n str = let ws = words str in chain (mkModel n ws) (take n ws)

type Model = Map [String] (Map String Int)

mkModel :: Int -> [String] -> Model
mkModel n items = train n items $ M.fromList []

train :: Int -> [String] -> Model -> Model
train n items model = foldl observe' model subs
  where subs = concat [sublists i items | i <- [2..n+1]]
        observe' mdl es = let (ks,ev) = (,) <$> init <*> last $ es
                          in observe ks ev mdl

-- | Record a state transition, updating the model.
observe :: [String] -> String -> Model -> Model
observe ks ev model = M.insert ks updated model
  where updated = M.insert ev count counts
        count   = succ $ fromMaybe 0 $ M.lookup ev counts
        counts  = fromMaybe mkCounter $ M.lookup ks model

chain :: RandomGen t => Model -> [String] -> t -> [String]
chain model state rng = case select (nxts state model) rng of
  Nothing -> []
  Just (nxt,rng') -> nxt:(chain model (tail state ++ [nxt]) rng')

-- | Return an empty event counter for weighting state transitions.
mkCounter :: Map String Int
mkCounter = M.fromList []

-- | Return the length-k sublists of xs, in order.
sublists :: Int -> [a] -> [[a]]
sublists k xs = [map (a!) [i..i+k-1] | i <- [0..n-k]]
  where a = listArray (0, n-1) xs
        n = length xs

-- | Return the weighted options for the next output from a state.
nxts :: [String] -> Model -> [(String,Int)]
nxts [] _ = []
nxts ws d = (M.toList . fromMaybe mkCounter . M.lookup ws $ d) ++ nxts (tail ws) d

-- | Sample from weighted options with probability proportional to weight.
-- String could be a type variable instead, right?
-- Could Int be more generic too?
select :: RandomGen t => [(String,Int)] -> t -> Maybe (String,t)
select []   _   = Nothing
select opts rng = Just (snd $ foldl1 sel opts', rng')
  where wts = scanl1 (+) $ map snd opts
        opts' = wts `zip` map fst opts
        sel s s' = if fst s' > n then s else s'
        (n, rng') = randomR (1,last wts) rng

