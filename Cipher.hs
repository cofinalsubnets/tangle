module Cipher
( encMono
, decMono
, encVig
, decVig
, runCipher
) where

import Base
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import System.Console.GetOpt
import System.Environment
import System.Exit

letters  = filter isLetter
downcase = map toLower

keyedAlphabet :: String -> String
keyedAlphabet = nub . (++ lcAlpha) . downcase . letters

caseTranslate     :: String -> String -> String -> String
caseTranslate i o = ((translate `on` (map toUpper)) i o) . ((translate `on` (map toLower)) i o)

vigKeys :: String -> [String]
vigKeys = map shiftedKey . downcase . letters
  where shiftedKey c = shift n lcAlpha
          where    n = fromMaybe 0 $ elemIndex (toLower c) lcAlpha

encMono :: String -> String -> String
encMono = caseTranslate lcAlpha          . keyedAlphabet

decMono :: String -> String -> String
decMono = (flip caseTranslate $ lcAlpha) . keyedAlphabet

encVig     :: String -> String -> String
encVig key = concatMap trans . (zip $ cycle $ vigKeys key) . map (:[])
  where trans (k, s) = caseTranslate lcAlpha k s

decVig     :: String -> String -> String
decVig key = concatMap trans . (zip $ cycle $ vigKeys key) . map (:[])
  where trans (k, s) = caseTranslate k lcAlpha s


-- command line interaction

data Cipher = Mono | Vig

cipher :: Cipher -> Bool -> String -> String -> String
cipher c d = case (c, d) of (Mono, False) -> encMono
                            (Mono, True)  -> decMono
                            (Vig,  False) -> encVig
                            (Vig,  True)  -> decVig

data Options = Options { ciph   :: Cipher
                       , deciph :: Bool
                       , key    :: String
                       , rounds :: Int
                       }

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

parseArgs :: IO Options
parseArgs = do
  args <- getArgs
  case getOpt Permute options args of
    (o,[],[]) -> return $ foldr ($) defaultOpts o
    (_,ns,es) -> do
      let unrecs = map (\n -> "Unknown argument: " ++ n ++ "\n") ns
          header = "Usage: tangle -c [OPTIONS...]"
          usage  = usageInfo header options
      mapM_ putStr (unrecs ++ es)
      putStr usage
      exitFailure

runCipher = do Options c d k r <- parseArgs
               let fn = (!!r) . (iterate $ cipher c d k)
               getContents >>= putStr . fn

main = runCipher
