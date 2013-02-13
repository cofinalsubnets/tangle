import System.Environment
import System.Exit
import qualified Tangle.Rot.Run    as R
import qualified Tangle.Cipher.Run as C
import qualified Tangle.Mangle.Run as M

main = do
  args <- getArgs
  case args of []     -> putStrLn usage
               (a:as) -> case lookup a actionMap of Just fn -> fn as
                                                    Nothing -> do putStrLn usage
                                                                  exitFailure
  where usage     = "Usage: tangle (r|c|m) [OPTIONS...]"
        actionMap = [ ("r", R.run), ("c", C.run), ("m", M.run)]

