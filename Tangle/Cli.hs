module Tangle.Cli
( Op(..)
, Arg(..)
, Tangle(..)
, for
) where

import Tangle.Rot
import Tangle.Mangle
import Tangle.Cipher
import Control.Monad
import Data.Char

data Arg = Arg { pos :: [String] , opt :: [(String, String)] }

class Show a => Tangle a where
  positional :: a -> Int
  optional   :: a -> [String]
  with       :: a -> Arg -> (Either String (String -> String))

data Op = Rot | Mangle | Cipher deriving (Show, Eq)

instance Tangle Op where

  positional Rot    = 1
  positional Mangle = 0
  positional Cipher = 2

  optional Rot    = []
  optional Mangle = ["chunk-size", "merge-size"]
  optional Cipher = ["rounds"]

  Rot `with` (Arg [n] _) = case maybeInt n of
    Just i  -> Right $ caesar i
    Nothing -> Left $ "-- Unable to parse `" ++ n ++ "'"

  Mangle `with` (Arg _ ps) = do 
    merge <- (lookup "merge-size" ps) `dflt` 1
    chunk <- (lookup "chunk-size" ps) `dflt` 4
    return $ mangle chunk merge

  Cipher `with` (Arg [ciph, key] rs) = do
    c <- getCipher ciph
    n <- (lookup "rounds" rs) `dflt` 1
    let cipher = if n < 1
                 then snd c
                 else fst c
        rounds = abs n
    return $ (!!rounds) . (iterate $ cipher key)
    where getCipher ciph = case lookup ciph ciphMap of
                             Nothing -> Left $ "-- Don't know about cipher `" ++ ciph ++ "'"
                             Just c  -> Right c
          ciphMap = [ ("vig",  (encVig, decVig))
                    , ("mono", (encMono, decMono)) ]


dflt Nothing n  = Right n
dflt (Just s) _ = case maybeInt s of Just i  -> Right i
                                     Nothing -> Left $ "-- Unable to parse `" ++ s ++ "'"

maybeInt :: String -> Maybe Int
maybeInt s = case s of
  []     -> Nothing
  '-':cs -> readIfDigits cs
  otherwise -> readIfDigits s
  where readIfDigits s' = if all isDigit s'
                          then Just (read s :: Int)
                          else Nothing

for :: Tangle a => [String] -> a -> (Either String Arg)
args `for` actn = do
  (reqd, as) <- reqdArgs args
  opts       <- optArgList as actn
  return $ Arg reqd opts

  where reqdArgs as = if pos > length as
                      then Left $ "-- Wrong number of positional arguments to " ++ (show actn) ++ " (" ++ (show $ length as) ++ " for " ++ show pos ++ ")"
                      else Right $ splitAt pos as
        pos = positional actn
        opt = optional   actn

        optArgList args actn
          | odd $ length args = Left "-- Odd number of optional arguments"
          | otherwise = if all (\ e -> elem e opts) $ map fst kvs
                        then Right $ kvs
                        else Left $ "-- Unrecognized optional arguments to " ++ show actn
          where kvs = ekvs args
                ekvs [] = []
                ekvs (k:v:l) = (k,v):(ekvs l)
                opts = optional actn


