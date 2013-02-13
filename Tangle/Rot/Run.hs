module Tangle.Rot.Run (run) where

import Tangle.Rot
import System.Exit

run args = case args of [n] -> getContents >>= putStr . (caesar $ read n)
                        otherwise -> putStrLn usage >> exitFailure
  where usage = "Usage: tangle -r N"

