import Tangle
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Random

main :: IO ()
main = do rng <- getStdGen
          Options c <- getArgs >>= parseArgs
          txt <- getContents 
          putStrLn . unwords . take c $ mangle txt (head $ words txt) rng

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

data Options = Options { maxWords :: Int }

defaultOpts :: Options
defaultOpts = Options { maxWords = 1000 }

options :: [OptDescr (Options -> Options)]
options = [ Option "w" ["words"] (ReqArg (\n opt -> opt { maxWords = read n }) "WORDS") "maximum number of words to generate" ]

