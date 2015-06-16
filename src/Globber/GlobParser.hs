module Globber.GlobParser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Globber.Glob

parseGlob :: String -> GlobPattern
parseGlob s =
  case parse globPattern "Glob Parser" s of
    Left e -> error . show $ e
    Right g -> g

globEscape :: Parser Glob
globEscape = do
  char '\\'
  c <- anyChar
  return . GlobLiteral $ c

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
  char '['
  s <- manyTill anyChar (try $ char ']')
  return . GlobRange $ s

globRangeFromTo :: Parser Glob
globRangeFromTo = do
  char '['
  a <- anyChar
  char '-'
  b <- anyChar
  char ']'
  return . GlobRange $ [a .. b]

globPattern :: Parser GlobPattern
globPattern = do
  g <- many $ globEscape
        <|> globAnyChar
        <|> globAnyString
        <|> try globRangeFromTo
        <|> globRange
        <|> globLiteral
  return . GlobPattern $ g
