module Tangle.Mangle.Run (run) where

import Tangle.Mangle
import System.Console.GetOpt
import System.Exit

run args = do Options c <- parseArgs args
              getContents >>= putStr . mangle c

parseArgs :: [String] -> IO Options
parseArgs args = do 
  case getOpt Permute options args of
    (o,[],[]) -> return $ foldr ($) defaultOpts o
    (_,ns,es) -> do
      let unrecs = map (\n -> "Unknown argument: " ++ n ++ "\n") ns
          header = "Usage: tangle -m [OPTIONS...]"
          usage  = usageInfo header options
      mapM_ putStr (unrecs ++ es)
      putStr usage
      exitFailure

data Options = Options { chunkSize :: Int }

defaultOpts :: Options
defaultOpts = Options { chunkSize = 4 }

options :: [OptDescr (Options -> Options)]
options = [ Option "c" ["chunk-size"] (ReqArg (\n opt -> opt { chunkSize = read n }) "CHUNK") "chunk size" ]

