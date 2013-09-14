import Tangle
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Console.GetOpt
import System.Random (getStdGen)

data Options = Options { maxWords  :: Int, hist :: Int, files :: [String] }

main :: IO ()
main = getArgs >>= parseArgs >>= go
 where
   go (Options w h fs) = do
     txt <- if null fs then getContents
            else fmap unwords $ mapM readFile' fs
     rng <- getStdGen
     putStrLn . unwords . take w $ mangle h (words txt) rng

   readFile' "-" = getContents
   readFile' f   = readFile f

parseArgs :: [String] -> IO Options
parseArgs args = case getOpt Permute options args of
  (o,fs,[]) -> return $ (foldr ($) defaults o) { files = fs }
  (_,_,err) -> mapM_ putStr err >> putStr usage >> exitFailure

defaults :: Options
defaults = Options { maxWords = 1000, hist = 1, files = [] }

usage :: String
usage = usageInfo header options
  where header = "Usage: tangle [OPTIONS...] [FILES...]"

options :: [OptDescr (Options -> Options)]
options = [ Option "w" ["words"]
              (ReqArg (\n opt -> opt { maxWords = read n }) "LENGTH")
              "maximum length of output"
          , Option "n" ["maximum-history"]
              (ReqArg (\n opt -> opt { hist = read n }) "N")
              "maximum ngram size to generate"
          ]

