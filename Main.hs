import Tangle.Cli
import System.Environment
import System.Exit

main = getArgs >>= handleArgs
  where

    handleArgs :: [String] -> IO ()
    handleArgs [] = putStrLn "-- Usage: tangle (rot n | mangle [params] | cipher ciph key [params] )"
    handleArgs (a:as) = case action of
                          Left err -> putStrLn err >> exitFailure
                          Right fn -> getContents >>= \ txt -> putStr $ fn txt
      where

        action = do actn <- getActn a
                    args <- as `for` actn
                    actn `with` args

        getActn a = let actMap = [ ("rot",    Rot)
                                 , ("mangle", Mangle) 
                                 , ("cipher", Cipher) ]
                    in case lookup a actMap of
                         Nothing -> Left $ "-- Unknown command `" ++ a ++ "'"
                         Just c  -> Right c

