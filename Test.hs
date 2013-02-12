#!/usr/bin/env runhaskell
import Base
import Rot
import Cipher
import Mangle

test          :: String -> [(Bool, String)] -> IO ()
test name exs = do putStrLn $ " -- " ++ name
                   mapM_ out exs

  where out       :: (Bool, String) -> IO ()
        out (p,m) = putStrLn $ (if p then "+ " else "- ") ++ m

baseExamples = [ ((ucTrans ohai)    == "OH HAI", "translate to upper case")
               , ((lcTrans ohai)    == "oh hai", "translate to lower case") 
               , ((noTrans ohai)    == ohai,     "null translate"         )
               , ((shift 1 ohai)    == "h HaiO", "positive shift"         )
               , ((shift (-1) ohai) == "iOh Ha", "negative shift"         )
               ]
  where
    ucTrans = translate lcAlpha ucAlpha
    lcTrans = translate ucAlpha lcAlpha
    noTrans = translate [] []
    ohai    = "Oh Hai"

rotExamples = [ ((rot13 "abjureR"  ) == "nowherE", "rot13"                )
              , ((rot13' "Barb56"  ) == "Oneo01",  "rot13 with rot5"      )
              , ((caesar 1 "abc"   ) == "bcd",     "positive caesar shift")
              , ((caesar (-1) "abc") == "zab",     "negative caesar shift")
              ]

exampleGroups = [ ("Base", baseExamples)
                , ("Rot",  rotExamples )
                ]

main = mapM_ (\ (n,xs) -> test n xs) exampleGroups

