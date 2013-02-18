import Tangle
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Random

main :: IO ()
main = do rng <- getStdGen
          Options w h <- getArgs >>= parseArgs
          txt <- getContents 
          putStrLn . unwords . take w $ mangle h txt (take h $ words txt) rng

parseArgs :: [String] -> IO Options
parseArgs args = do 
  case getOpt Permute options args of
    (o,[],[]) -> return $ foldr ($) defaultOpts o
    (_,ns,es) -> do
      let unrecs = map (\n -> "Unknown argument: " ++ n ++ "\n") ns
          header = "Usage: tangle [OPTIONS...]"
          usage  = usageInfo header options
      mapM_ putStr (unrecs ++ es)
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

