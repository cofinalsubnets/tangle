{-# LANGUAGE TupleSections #-}
module Trie.Count
( CountTrie(..)
, mkTrie
, insert
, counts
, keys
, keysFrom
, listFrom
, toList
, delete
, lookup
) where

import Prelude hiding (lookup)
import Data.Maybe
import qualified Trie.Map as TM

-- | A specialized TrieMap for maintaining list counts.
newtype CountTrie a = CountTrie (TM.TrieMap a Int) deriving (Show, Eq)

-- | A new empty CountTrie.
mkTrie :: Ord a => CountTrie a
mkTrie = CountTrie TM.mkTrie

-- | Insert a list into the trie. Increments the count if the list is already
-- present.
insert :: Ord a => [a] -> CountTrie a -> CountTrie a
insert w (CountTrie tm) = CountTrie $ TM.insertWith (+) w 1 tm

-- | A list of lists present in the trie.
keys :: Ord a => CountTrie a -> [[a]]
keys = map fst . toList

-- | Dual to keys - a list of counts ordered such that 
-- keys t `zip` counts t == toList t
counts :: Ord a => CountTrie a -> [Int]
counts = map snd . toList

-- | Return the count associated with a key.
lookup :: Ord a => [a] -> CountTrie a -> Int
lookup k (CountTrie tm) = fromMaybe 0 $ TM.lookup k tm

-- | A list of list-count pairs present in the trie.
toList :: Ord a => CountTrie a -> [([a],Int)]
toList (CountTrie tm) = TM.toList tm

-- | Decrement the count for the given list in the given tree by one. If the
-- new count is zero, delete the branch entirely.
delete :: Ord a => [a] -> CountTrie a -> CountTrie a
delete w (CountTrie tm) = let updated = TM.adjust (subtract 1) w tm
                          in if TM.lookup w updated == Just 0
                             then CountTrie $ TM.delete w updated
                             else CountTrie updated

keysFrom :: Ord a => [a] -> CountTrie a -> [[a]]
keysFrom w (CountTrie tm) = TM.keysFrom w tm

listFrom :: Ord a => [a] -> CountTrie a -> [([a],Int)]
listFrom w (CountTrie tm) = TM.listFrom w tm

