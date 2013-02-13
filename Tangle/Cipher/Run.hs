module Tangle.Cipher.Run (run) where

import Tangle.Cipher
import System.Console.GetOpt
import System.Exit

run      :: [String] -> IO ()
run args = do Options c d k r <- parseArgs args
              let run = (!!r) . (iterate $ cipher c d k)
              getContents >>= putStr . run

data Options = Options { ciph   :: Cipher
                       , deciph :: Bool
                       , key    :: String
                       , rounds :: Int
                       }

data Cipher = Mono | Vig

cipher :: Cipher -> Bool -> String -> String -> String
cipher Mono False = encMono
cipher Mono True  = decMono
cipher Vig  False = encVig
cipher Vig  True  = decVig


options :: [OptDescr (Options -> Options)]
options = [ Option "k" ["key"]
              (ReqArg (\k opt -> opt { key = k }) "KEY")
              "encryption key"
          , Option "d" ["decipher"]
              (NoArg (\opt -> opt { deciph = True }))
              "decipher instead of enciphering"
          , Option "v" ["vigenere"]
              (NoArg (\opt -> opt { ciph = Vig }))
              "vigenere cipher"
          , Option "m" ["monoalphabetic"]
              (NoArg (\opt -> opt { ciph = Mono}))
              "phrase-keyed monoalphabetic cipher"
          , Option "r" ["rounds"]
              (ReqArg (\n opt -> opt { rounds = read n }) "ROUNDS")
              "rounds of application"
          ]

defaultOpts :: Options
defaultOpts = Options Mono False "" 1

parseArgs      :: [String] -> IO Options
parseArgs args = do
  case getOpt Permute options args of
    (o,[],[]) -> return $ foldr ($) defaultOpts o
    (_,ns,es) -> do
      let unrecs = map (\n -> "Unknown argument: " ++ n ++ "\n") ns
          header = "Usage: tangle -c [OPTIONS...]"
          usage  = usageInfo header options
      mapM_ putStr (unrecs ++ es)
      putStr usage
      exitFailure

