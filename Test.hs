#!/usr/bin/env runhaskell
import Base
import Rot
import Mangle

test          :: String -> [(Bool, String)] -> IO ()
test name exs = do putStrLn $ " -- " ++ name
                   mapM_ out exs

  where out       :: (Bool, String) -> IO ()
        out (p,m) = putStrLn $ (if p then "+ " else "- ") ++ m

exampleGroups = [ ("Base", baseExamples)
                , ("Rot",  rotExamples )
                , ("Mangle", mangleExamples)
                ]

main = mapM_ (\ (n,xs) -> test n xs) exampleGroups

