#!/usr/bin/env runhaskell
import Data.Char
import Data.List
import Data.Maybe
import Test.QuickCheck
import qualified Trie.Map as TM
import qualified Trie.Count as TC

mTrie :: Ord a => TM.TrieMap a b
mTrie = TM.mkTrie

cTrie :: Ord a => TC.CountTrie a
cTrie = TC.mkTrie

prop_valClobber :: String -> Int -> Int -> Bool
prop_valClobber k v v' = TM.lookup k trie' == Just v'
  where trie  = TM.insert k v mTrie
        trie' = TM.insert k v' trie

prop_valDel :: String -> Bool
prop_valDel k = not $ k `TM.member` trie'
  where trie  = TM.insert k 1 mTrie
        trie' = TM.delete k trie

prop_valSet :: String -> Int -> Bool
prop_valSet k v = TM.lookup k trie == Just v
  where trie = TM.insert k v mTrie

prop_countInc :: String -> Bool
prop_countInc k = TC.lookup k trie == 2
  where ins = TC.insert k
        trie = ins . ins $ cTrie

prop_countDec :: String -> Bool
prop_countDec k = TC.lookup k trie == 1
  where ins = TC.insert k
        del = TC.delete k
        trie = del . ins . ins $ cTrie

prop_countZero :: String -> Bool
prop_countZero k = TC.lookup k cTrie == 0

prop_mapNonKey :: String -> Bool
prop_mapNonKey k = TM.lookup k trie == Nothing
  where trie = mTrie :: TM.TrieMap Char Char

prop_keysFrom k0 k1 k2 = uniq k0 k1 k2 && prefix k0 k1 k2 ==> correct $ TM.keysFrom k0 trie
  where ins k = TM.insert k True
        correct ks = ks == sort [k1, k2]
        trie = ins k2 $ ins k1 $ ins k0 mTrie
        uniq a b c = a /= b && b /= c && a /= c
        prefix a b c = isPrefixOf a b && isPrefixOf a c
        types = k0 :: [()]

main = do quickCheck prop_valClobber
          quickCheck prop_valDel
          quickCheck prop_valSet
          quickCheck prop_countInc
          quickCheck prop_countDec
          quickCheck prop_countZero
          quickCheck prop_mapNonKey
          quickCheck prop_keysFrom
         
