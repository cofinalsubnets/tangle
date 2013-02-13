module Tangle.Cipher
( encMono
, decMono
, encVig
, decVig
) where

import Tangle.Base
import Data.Char
import Data.List
import Data.Maybe
import Data.Function

letters  :: String -> String
letters  = filter isLetter

downcase :: String -> String
downcase = map toLower

keyedAlphabet :: String -> String
keyedAlphabet = nub . (++ lcAlpha) . downcase . letters

caseTranslate     :: String -> String -> String -> String
caseTranslate i o = ((translate `on` (map toUpper)) i o) . ((translate `on` (map toLower)) i o)

vigKeys :: String -> [String]
vigKeys = map shiftedKey . downcase . letters
  where shiftedKey c = shift n lcAlpha
          where    n = fromMaybe 0 $ elemIndex (toLower c) lcAlpha

encMono :: String -> String -> String
encMono = caseTranslate lcAlpha          . keyedAlphabet

decMono :: String -> String -> String
decMono = (flip caseTranslate $ lcAlpha) . keyedAlphabet

encVig     :: String -> String -> String
encVig key = concatMap trans . (zip $ cycle $ vigKeys key) . map (:[])
  where trans (k, s) = caseTranslate lcAlpha k s

decVig     :: String -> String -> String
decVig key = concatMap trans . (zip $ cycle $ vigKeys key) . map (:[])
  where trans (k, s) = caseTranslate k lcAlpha s

