module Globber (matchGlob) where

import Globber.GlobParser
import Globber.Glob

-- | Matches a glob on a string.
--
-- >>> matchGlob "abcd" "abcd"
-- True
--
-- >>> matchGlob "a[b-z]c" "adc"
-- True
--
-- >>> matchGlob "a[abcd]c" "abc"
-- True
--
-- >>> matchGlob "a[xyz]c" "abc"
-- False
--
-- >>> matchGlob "a?c" "adc"
-- True
--
-- >>> matchGlob "/usr/*/stuff" "/usr/glob/stuff"
-- True
--
-- >>> matchGlob "/usr/*/stuff" "/usr/stuff"
-- False
--
-- >>> matchGlob "\\a\\b\\c\\d\\e\\[]" "abcde[]"
-- True
--
-- >>> matchGlob "da?id*[1-9]" "davidcool3"
-- True
--
-- >>> matchGlob "da?id*[1-9]" "davidcool"
-- False
matchGlob :: String -> String -> Bool
matchGlob glob input =
  match input . parseGlob $ glob
