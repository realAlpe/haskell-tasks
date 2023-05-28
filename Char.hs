module Char (equal, isNumeral, isBlank, shift) where

import Data.Char

--------------------------------------------------------------------------------
-- a)

equal :: String -> String -> Bool
equal s1 s2 = map toLower s1 == map toLower s2

--------------------------------------------------------------------------------
-- b)
isNumeral :: String -> Bool
isNumeral = all isDigit

-- isNumeral s = and (map (\x -> x >= '0' && x <= '9') s)

isBlank :: String -> Bool
isBlank = all (== ' ')

-- isBlank s = and (map (\x -> x == ' ') s)

--------------------------------------------------------------------------------
-- c)

shift :: Int -> Char -> Char
shift n c =
  if c == ' '
    then ' '
    else chr (ord 'a' + (n + ord (toLower c) - ord 'a') `mod` 26)

msg :: String
msg =
  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ "
    ++ "JHLJBZ KPJABT HYJHUBT LZA ULBAYVU"

-- decoded with: shift 19
-- message is: faber est suae quisque fortunae appius claudius caecus dictum arcanum est neutron
