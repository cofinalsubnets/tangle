import Mangle
import Rot
import Cipher
import System.Environment
import System.Exit

main = do
  args <- getArgs
  case args of []     -> putStrLn usage
               (a:as) -> case lookup a actionMap of Just fn -> withArgs as fn
                                                    Nothing -> do putStrLn usage
                                                                  exitFailure
  where usage     = "Usage: tangle (r|c|m) [OPTIONS...]"
        actionMap = [ ("r", runRot), ("c", runCipher), ("m", runMangle)]

