import Tangle
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Console.GetOpt
import System.Random (getStdGen)

main :: IO ()
main = getArgs >>= parseArgs >>= go
  where
    go (Options w h fs) = do
      txt <- if null fs then getContents
             else fmap unwords $ mapM readFile fs
      rng <- getStdGen
      putStrLn . unwords . take w $ mangle h (words txt) rng

parseArgs :: [String] -> IO Options
parseArgs args = case getOpt Permute options args of
  (o,fs,[]) -> return $ (foldr ($) defaults o) { files = fs }
  (_,_,err) -> mapM_ putStr err >> putStr usage >> exitFailure

data Options = Options { maxWords  :: Int, ngramSize :: Int, files :: [String] }

defaults :: Options
defaults = Options { maxWords = 1000, ngramSize = 2, files = [] }

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

