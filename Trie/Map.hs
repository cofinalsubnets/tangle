{-# LANGUAGE TupleSections #-}
module Trie.Map
( TrieMap(..)
, mkTrie
, lookup
, insert
, insertWith
, delete
, adjust
, member
, keys
, elems
, keysFrom
, elemsFrom
, listFrom
, fromList
, toList
) where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Monoid
import Control.Monad
import qualified Data.Map as M

-- | A generalized trie data structure capable of storing key-value pairs.
data TrieMap a b = Node { value :: Maybe b
                        , nodes :: M.Map a (TrieMap a b)
                        } deriving (Eq)

-- | An empty root node. Equivalent to mempty.
mkTrie :: Ord a => TrieMap a b
mkTrie = Node Nothing $ M.fromList []

-- | Given a key and a trie, if the key is present in the trie, return its
-- value wrapped in Just; otherwise return Nothing.
lookup          :: Ord a => [a] -> TrieMap a b -> Maybe b
lookup [] n     = value n
lookup (c:cs) n = M.lookup c (nodes n) >>= lookup cs

-- | Insert the given key-value pair into the trie. If the key already exists,
-- replace its value.
insert :: Ord a => [a] -> b -> TrieMap a b -> TrieMap a b
insert = insertWith const

-- | Apply the given function to the given value and the value associated with
-- the given key, and associate the result with the given key. If the key is
-- not already present in the trie, then associate the given value with the
-- key.
insertWith :: Ord k => (v -> v -> v) -> [k] -> v -> TrieMap k v -> TrieMap k v
insertWith f [] v n = n { value = nVal }
  where nVal = case value n of Nothing -> Just v
                               Just v' -> Just $ f v v'

insertWith f (c:cs) v n = updated
  where updated   = n { nodes = M.insert c (insertWith f cs v subtrie) ns }
        subtrie   = fromMaybe mkNode $ M.lookup c ns
        ns        = nodes n

-- | Delete the key and its value from the given trie. If the key is not
-- present in the trie, return the trie unaltered.
delete     :: Ord a => [a] -> TrieMap a b -> TrieMap a b
delete k n = fromMaybe n $ mdel
  where mdel = do (bn,bcs) <- zipWd k $ mkZip n
                  return . fst $ zipTop (bn { value = Nothing }, bcs)

-- | Apply the given function to the value of the given key, then return an
-- updated trie. If the key is not present in the trie, return the original
-- trie unchanged.
adjust :: Ord k => (v -> v) -> [k] -> TrieMap k v -> TrieMap k v
adjust f k tm = fromMaybe tm updated
  where updated = do (n,cs) <- zipWd k $ mkZip tm
                     let val = fmap f $ value n
                     return . fst . zipTop $ (n { value = val }, cs)
 
-- | Given a key and a trie, return whether the key is present in the trie.
member   :: Ord a => [a] -> TrieMap a b -> Bool
member w = isJust . lookup w

-- | Return a list of unique keys present in the given trie.
keys :: Ord a => TrieMap a b -> [[a]]
keys = map fst . toList

elems :: Ord a => TrieMap a b -> [b]
elems = map snd . toList

-- | Return a list of keys beginning with the given prefix. If the prefix is
-- a key, it will not be included in the collection returned.
keysFrom      :: Ord a => [a] -> TrieMap a b -> [[a]]
keysFrom w tm = map (w++) sks
  where sks = fromMaybe [] $ do (n,_) <- zipWd w $ mkZip tm
                                return $ keys n

-- | Return a list of values stored beneath the node named by the supplied
-- key. If the given key is associated with a value, that value will not be
-- included in the list.
elemsFrom :: Ord a => [a] -> TrieMap a b -> [b]
elemsFrom w tm = fromMaybe [] $ do (n,_) <- zipWd w $ mkZip tm
                                   return $ elems n

-- | Return a list containing the key-value pairs stored beneath the supplied
-- key in the trie. If the given key is part of a key-value pair, that pair
-- will not be included in the list.
listFrom :: Ord a => [a] -> TrieMap a b -> [([a],b)]
listFrom w tm = fromMaybe [] lst
  where lst = fmap fst (zipWd w $ mkZip tm) >>= Just . toList

-- | Build a trie from a list of key-value pairs.
fromList :: Ord a => [([a],b)] -> TrieMap a b
fromList = mergeWithList mempty

-- | Return the contents of the trie as a list of key-value pairs.
toList   :: Ord a => TrieMap a b -> [([a],b)]
toList = toList' . mkZip
  where
    toList'          :: Ord a => Zip a b -> [([a],b)]
    toList' z@(n,ps) = w ++ nxt
      where w   = if null ps -- Necessary to exclude the root when
                  then []    -- beginning the search in a proper subtree.
                  else case n of Node (Just v) _ -> [(zWord z, v)]
                                 _ -> []
            nxt = concatMap toList' $ catMaybes $ map (flip zipDn $ z) $ M.keys $ nodes n

-- | Return the key leading to the current code of the given zipper.
zWord :: Ord a => Zip a b -> [a]
zWord = reverse . zw
  where zw (_,[])     = []
        zw z@(_,c:_) = (cdex c):(zw $ fromJust $ zipUp z)

-- | Merge a trie with a list. toList and mappend both depend on this function.
mergeWithList   :: Ord a => TrieMap a b -> [([a],b)] -> TrieMap a b
mergeWithList n = foldl ins n
  where ins t (k,v) = insert k v t

-- | Return a new empty node.
mkNode :: Ord a => TrieMap a b
mkNode = Node Nothing $ M.fromList []

-- | Instances
instance Ord a => Monoid (TrieMap a b) where
  mempty    = mkTrie
  mappend a = mergeWithList a . toList

instance Ord a => Functor (TrieMap a) where
  fmap f n@(Node v ns) = n { value = fmap f $ v
                           , nodes = M.map (fmap f) ns
                           }

instance (Ord a, Show a, Show b) => Show (TrieMap a b) where
  show = ("fromList " ++) . show . toList

-- | Node context type for trie traversal via a zipper.
data Context a b = Context { cval :: Maybe b 
                           , cdex :: a
                           , nmap :: M.Map a (TrieMap a b)
                           } deriving (Show, Eq)

-- | Zipper for trie traversal.
type Zip a b = (TrieMap a b, [Context a b])

-- | Wrap a node in a zipper.
mkZip :: Ord a => TrieMap a b -> Zip a b
mkZip = (,[])

-- | Attempt to move up one level in the trie.
zipUp   :: Ord a => Zip a b -> Maybe (Zip a b)
zipUp (_,[])   = Nothing
zipUp (n,p:ps) = Just (node, ps)
  where node = Node (cval p) nodemap
        nodemap = case n of Node Nothing m -> if M.null m then nmap p else augmap
                            _ -> augmap
        augmap = M.insert (cdex p) n (nmap p)

-- | Zip to the root of the trie.
zipTop   :: Ord a => Zip a b -> Zip a b
zipTop z = case zipUp z of Nothing -> z
                           Just z' -> zipTop z'

-- | Attempt to move down one level in the trie.
zipDn          :: Ord a => a -> Zip a b -> Maybe (Zip a b)
zipDn k (Node v ns,ps) = M.lookup k ns >>= return . (,context:ps)
  where context = Context v k nodemap
        nodemap = M.delete k ns

-- | Attempt to zip down several levels by following the given key from the 
-- current node.
zipWd     :: Ord a => [a] -> Zip a b -> Maybe (Zip a b)
zipWd w z = foldM (flip zipDn) z w

