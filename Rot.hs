module Rot
( rot
, rot13
, rot13'
, rot5
, rot47
, caesar
, runRot
) where

import Base
import Data.Char
import System.Environment
import System.Exit

rot            :: Eq a => Int -> [a] -> [a] -> [a]
rot n alphabet = translate alphabet $ shift n alphabet

caesar   :: Int -> String -> String
caesar n = (rot n ['A'..'Z']) . (rot n ['a'..'z'])

rot13 :: String -> String
rot13 = caesar 13

rot5 :: String -> String
rot5 = rot 5 ['0'..'9']

rot13' :: String -> String
rot13' = rot13 . rot5

rot47  :: String -> String
rot47  = rot 47 $ sevenBitPrintable

runRot = do args <- getArgs
            case args of [n] -> getContents >>= putStr . (caesar $ read n)
                         otherwise -> putStrLn usage >> exitFailure
            where usage = "Usage: tangle -r N"

