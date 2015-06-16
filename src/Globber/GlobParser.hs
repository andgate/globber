module Globber.GlobParser where

import Text.ParserCombinators.Parsec
import Globber.Glob

parseGlob :: String -> Glob
parseGlob s =
  case parse globPattern "Glob Parser" s of
    Left e -> error . show $ e
    Right g -> g

globLiteral :: Parser Glob
globLiteral = do
  c <- anyChar
  return $ GlobLiteral c

globAnyChar :: Parser Glob
globAnyChar = do
  char '?'
  return GlobAnyChar

globAnyString :: Parser Glob
globAnyString = do
  char '*'
  return GlobAnyString

globRange :: Parser Glob
globRange = do
  s <- between (char '[') (char ']') (many $ fromToParser <|> anyCharS)
  return . GlobRange . unlines $ s

anyCharS :: Parser String
anyCharS = do
  c <- anyChar
  return [c]

fromToParser :: Parser String
fromToParser = do
  a <- anyChar
  char '-'
  b <- anyChar
  return [a .. b]

globPattern :: Parser Glob
globPattern = do
  g <- many $ globLiteral
        <|> globAnyChar
        <|> globAnyString
        <|> globRange
  return . GlobPattern $ g
