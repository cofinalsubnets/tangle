import Tangle
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Random
import Control.Monad

main :: IO ()
main = do rng <- getStdGen
          (Options w h, files) <- getArgs >>= parseArgs
          txt <- if null files
                 then getContents
                 else liftM unwords $ mapM readFile files
          putStrLn . unwords . take w $ mangle h txt rng

parseArgs :: [String] -> IO (Options,[String])
parseArgs args = do 
  case getOpt Permute options args of
    (o,ns,[]) -> return $ (foldr ($) defaultOpts o, ns)
    (_,_,es) -> do
      let header = "Usage: tangle [OPTIONS...] [FILES...]"
          usage  = usageInfo header options
      mapM_ putStr es
      putStr usage
      exitFailure

data Options = Options { maxWords :: Int
                       , history  :: Int
                       }

defaultOpts :: Options
defaultOpts = Options { maxWords = 1000 
                      , history  = 1
                      }

options :: [OptDescr (Options -> Options)]
options = [ Option "w" ["words"] 
              (ReqArg (\n opt -> opt { maxWords = read n }) "WORDS")
              "maximum number of words to generate" 
          , Option "h" ["history"]
              (ReqArg (\n opt -> opt { history = read n }) "HISTORY")
              "number of previous states to retain" 
          ]

