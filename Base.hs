module Base
( translate
, shift
, lcAlpha
, ucAlpha
, sevenBitPrintable
) where

import Data.Char

lcAlpha = ['a'..'z']
ucAlpha = ['A'..'Z']

sevenBitPrintable = map chr [32..126]

translate i o = map trans
  where trans c = case lookup c $ zip i o of Just c' -> c'
                                             Nothing -> c

shift n lst = take l $ drop (l + n) $ cycle lst
  where l = length lst

