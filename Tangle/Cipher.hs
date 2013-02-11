module Tangle.Cipher
( encMonoKey
, decMonoKey
, encVig
, decVig
) where

import Tangle.Base
import Data.Char
import Data.List
import Data.Maybe

alphabet  = ['a'..'z']

keyedAlphabet = nub . (++ alphabet) . lowercaseLetters

lowercaseLetters = map toLower . filter isLetter

caseTranslate i o = (translate (map toUpper i) (map toUpper o)) . (translate (map toLower i) (map toLower o))

vigKeys = map shiftedKey . lowercaseLetters
  where shiftedKey c = shift n alphabet
          where    n = fromMaybe 0 $ elemIndex (toLower c) alphabet

encMonoKey = caseTranslate alphabet          . keyedAlphabet
decMonoKey = (flip caseTranslate $ alphabet) . keyedAlphabet

encVig key = concatMap trans . (zip $ cycle $ vigKeys key) . map (:[])
  where trans (k, s) = caseTranslate alphabet k s

decVig key = concatMap trans . (zip $ cycle $ vigKeys key) . map (:[])
  where trans (k, s) = caseTranslate k alphabet s

