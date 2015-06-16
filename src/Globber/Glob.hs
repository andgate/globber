module Globber.Glob where

import Data.Char
import Data.List

data GlobPattern = GlobPattern [Glob]
  deriving(Show)

data Glob =
    GlobLiteral Char
    | GlobAnyChar
    | GlobAnyString
    | GlobRange String
    deriving(Show, Eq)

compareToGlob :: String -> GlobPattern -> Bool
compareToGlob _ (GlobPattern []) = False
compareToGlob s (GlobPattern gs) = snd $ foldl' (\(s, a) g -> let (s', b) = match s g in (s', a && b)) (s, True) gs


match :: String -> Glob -> (String, Bool)

match [] (GlobLiteral _) =
  ([], False)
match (a:s) (GlobLiteral c) =
  (s, a == c)

match [] (GlobAnyChar) =
  ([], False)
match (_:s) (GlobAnyChar) =
  (s, True)

match [] (GlobAnyString) =
  ([], True)
match s (GlobAnyString) =
  (dropWhile (`notElem` "\\/") s, True)

match [] (GlobRange _)
  = ([], False)
match (c:s) (GlobRange r) =
  (s, c `elem` r)
