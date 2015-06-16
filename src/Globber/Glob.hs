module Globber.Glob where

data Glob =
    GlobLiteral !Char
    | GlobAnyChar
    | GlobAnyString
    | GlobRange String
    | GlobPattern [Glob]
    deriving(Show, Eq)
