module ConsonantValue
  ( solve
  ) where

import Data.List

chars :: String
chars = "abcdefghijklmnopqrstuvwxyz"

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c =
  case c `elemIndex` vowels of
    Just _ -> True
    Nothing -> False

vowelSplit :: String -> String -> Int -> [String] -> [String]
vowelSplit str word index acc
  | index == (length str) = (word:acc)
  | isVowel c = vowelSplit str "" (index + 1) (word:acc)
  | otherwise = vowelSplit str (word ++ [c]) (index + 1) acc
  where c = str!!index

charScore :: Char -> Int
charScore c =
  case c `elemIndex` chars of
    Just n -> (n + 1)
    Nothing -> 0

wordScore :: String -> Int
wordScore str = foldl (\acc c -> acc + (charScore c)) 0 str

solve :: String -> Int
solve xs = (foldl (\acc s -> if s > acc then s else acc) 0) $ (map wordScore) $ (vowelSplit xs "" 0 [])