module BackspacesInString
  ( cleanString
  ) where

cleanString2 :: String -> Int -> String -> String
cleanString2 str index acc
  | index == (length str) = acc
  | c == '#' = cleanString2 str (index + 1) (if (length acc) > 0 then take ((length acc) - 1) acc else acc)
  | otherwise = cleanString2 str (index + 1) (acc ++ [c])
  where c = str!!index

cleanString :: String -> String
cleanString str = cleanString2 str 0 ""

{--
Tests:
cleanString "abc#d##c"
cleanString "abc##d######"
cleanString "#######"
cleanString ""
--}