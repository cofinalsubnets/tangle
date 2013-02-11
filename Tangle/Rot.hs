module Tangle.Rot
( rot
, rot13
, rot13'
, rot5
, rot47
, caesar
) where

import Tangle.Base
import Data.Char

rot :: Eq a => Int -> [a] -> [a] -> [a]
rot n alphabet = translate alphabet $ shift n alphabet

caesar :: Int -> String -> String
caesar n = (rot n ['A'..'Z']) . (rot n ['a'..'z'])

rot13 :: String -> String
rot13 = caesar 13

rot5 :: String -> String
rot5 = rot 5 ['0'..'9']

rot13' :: String -> String
rot13' = rot13 . rot5

rot47 :: String -> String
rot47  = rot 47 $ sevenBitPrintable


main = test examples
  where examples = [ ((rot13 "abjureR"  ) == "nowherE", "rot13"                )
                   , ((rot13' "Barb56"  ) == "Oneo01",  "rot13 with rot5"      )
                   , ((caesar 1 "abc"   ) == "bcd",     "positive caesar shift")
                   , ((caesar (-1) "abc") == "zab",     "negative caesar shift") ]

