module Globber.Glob where

import Data.Char
import Data.List

type GlobPattern = [Glob]

data Glob =
    GlobLiteral Char
    | GlobAnyChar
    | GlobAnyString
    | GlobRange String
    deriving(Show, Eq)

compareToGlob :: String -> GlobPattern -> Bool
compareToGlob _ [] = False
compareToGlob s gs = match s gs


match :: String -> GlobPattern -> Bool

match [] (GlobLiteral _ :_) = False
match (a:s) (GlobLiteral c : gs) =
  a == c && match s gs

match [] (GlobAnyChar : _) =
  False
match (_:s) (GlobAnyChar:gs) =
  match s gs

match [] (GlobAnyString:gs) =
  True && match [] gs
match (_:s) g@(GlobAnyString:gs) =
  match s gs || match s g

match [] (GlobRange _ : _) =
  False
match (c:s) (GlobRange r : gs) =
  c `elem` r && match s gs

match [] [] = True
match (_ : _) [] = False
