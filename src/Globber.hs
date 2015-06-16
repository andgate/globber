module Globber where

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
-- >>> matchGlob "a?c" "adc"
-- True
-- >>> matchGlob "/usr/*/stuff" "usr/glob/stuff"
-- True
--
-- >>> matchGlob "/usr/*/stuff" "usr/stuff"
-- False
--
-- >>> matchGlob "\a\b\c\d\e\[]" "abcde\[]"
-- True
matchGlob :: String -> String -> Bool
matchGlob glob input =
  checkGlob (parserGlob glob) input


checkGlob
