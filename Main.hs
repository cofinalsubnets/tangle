import Tangle
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Random

main :: IO ()
main = getArgs >>= parseArgs >>= go
  where
    go (Options w h, files) = do
      txt <- if null files then getContents
             else fmap unwords $ mapM readFile files
      rng <- getStdGen
      putStrLn . unwords . take w $ mangle h (words txt) rng

parseArgs :: [String] -> IO (Options,[String])
parseArgs args = case getOpt Permute options args of
  (o,ns,[]) -> return $ (foldr ($) defaults o, ns)
  (_,_,es)  -> mapM_ putStr es >> putStr usage >> exitFailure

data Options = Options { maxWords  :: Int, ngramSize :: Int }

defaults :: Options
defaults = Options { maxWords = 1000, ngramSize = 2 }

header :: String
header = "Usage: tangle [OPTIONS...] [FILES...]"

usage :: String
usage = usageInfo header options

options :: [OptDescr (Options -> Options)]
options = [ Option "w" ["words"] 
              (ReqArg (\n opt -> opt { maxWords = read n }) "LENGTH")
              "maximum length of output" 
          , Option "n" ["maximum-ngram-size"]
              (ReqArg (\n opt -> opt { ngramSize = read n }) "N")
              "maximum ngram size to generate" 
          ]

