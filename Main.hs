import Tangle
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Random

main = do rng <- getStdGen
          Options c <- getArgs >>= parseArgs
          getContents >>= putStrLn . mangle c rng

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

data Options = Options { chunkSize :: Int }

defaultOpts :: Options
defaultOpts = Options { chunkSize = 8 }

options :: [OptDescr (Options -> Options)]
options = [ Option "c" ["chunk-size"] (ReqArg (\n opt -> opt { chunkSize = read n }) "CHUNK") "chunk size" ]

