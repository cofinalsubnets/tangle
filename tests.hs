#!/usr/bin/env runhaskell
import Control.Monad (when)
import System.Exit

import Model
import Data.Map as M (fromList)


test :: Bool -> String -> IO Bool
test v fm = when (not v) (putStrLn fm) >> return v

main = do
  let tests = [gramsTest, modelTest, successorsTest]
  ok <- fmap and $ sequence tests
  when (not ok) exitFailure


gramsTest = test (result == expected) failureMsg
  where
    str = "abcdefg"
    expected = ["ab","bc","cd","de","ef","fg"]
    result = 2 `grams` str
    failureMsg = unwords ["grams 2", show str, "=", show result]

modelTest = test (mdl == expected) failureMsg
  where
    mdl = model 1 evs
    evs = [1..4]
    expected = M.fromList [([n], M.fromList [(n+1,1)]) | n <- [1..3]]
    failureMsg = unwords ["model 2", show evs, "=",show expected]

successorsTest = test (succs == expected) failureMsg
  where
    succs = successors mdl state
    state = [0]
    mdl = model 2 [1,0,2,3,0,4,0,2]
    expected = [(2,2),(4,1)]
    failureMsg = unwords ["successors",show mdl,show state,"=",show succs]


