module Worda10n
  ( abbreviate
  ) where

import Data.Char
-- import Data.List
-- import Data.List.Split

abbreviateConditional :: String -> String
abbreviateConditional str
  | len <= 3 = str
  | otherwise = [str!!0] ++ (show $ (len - 2)) ++ [str!!(len - 1)]
  where len = length str

abbreviate2 :: String -> String -> Int -> String -> String
abbreviate2 inp running index acc
  | index == (length inp) = acc ++ (abbreviateConditional running)
  | (not (isAlpha c)) && (length running) <= 3 = abbreviate2 inp "" (index + 1) (acc ++ running ++ [c])
  | (not (isAlpha c)) && (length running) > 3 = abbreviate2 inp "" (index + 1) (acc ++ (abbreviateConditional running) ++ [c])
  | otherwise = abbreviate2 inp (running ++ [c]) (index + 1) acc
  where c = inp!!index

abbreviate :: String -> String
abbreviate str = abbreviate2 str "" 0 ""