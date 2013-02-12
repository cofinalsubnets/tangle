module Mangle
( mangle
, runMangle
) where

import Data.List
import System.Console.GetOpt
import System.Exit
import System.Environment

data Options = Options { chunkSize :: Int
                       , mergeSize :: Int
                       }

defaultOpts :: Options
defaultOpts = Options { chunkSize = 4, mergeSize = 1 }

options :: [OptDescr (Options -> Options)]
options = [ Option "c" ["chunk"] (ReqArg (\n opt -> opt { chunkSize = read n }) "CHUNK") "chunk size"
          , Option "m" ["merge"] (ReqArg (\n opt -> opt { mergeSize = read n }) "MERGE") "merge size"
          ]

slices :: Int -> [a] -> [[a]]
slices _ [] = []
slices n ls = s : (slices n l)
  where (s, l) = splitAt n ls

synth :: Eq a => Int -> [[a]] -> [[a]]
synth _ [] = []
synth n (base:strs) = case synth' base strs of
  (Nothing,  _) -> synth n strs
  (Just m, rem) -> m:(synth n rem)

  where synth' base strs = case ms of []     -> (Nothing, rem)
                                      (w:ws) -> (Just $ prel ++ w, ws ++ rem)

          where (ms, rem)    = partition (isPrefixOf term) strs
                (prel, term) = splitAt (length base - n) base

mangle :: Int -> Int -> String -> String
mangle chunk merge = unwords . map unwords . synth merge . slices chunk . words

main = runMangle

runMangle = do Options c m <- parseArgs
               getContents >>= putStr . mangle c m

parseArgs :: IO Options
parseArgs = do 
  args <- getArgs
  case getOpt Permute options args of
    (o,[],[]) -> return $ foldr ($) defaultOpts o
    (_,ns,es) -> do
      let unrecs = map (\n -> "Unknown argument: " ++ n ++ "\n") ns
          header = "Usage: tangle -m [OPTIONS...]"
          usage  = usageInfo header options
      mapM_ putStr (unrecs ++ es)
      putStr usage
      exitFailure

