module MergedStringChecker
  ( isMerge
  , check
  ) where

check :: String -> String -> String -> Int -> Int -> String -> Bool
check s part1 part2 index1 index2 running
  | ((length part1) + (length part2)) /= (length s) = False
  | (index1 == (length part1)) && (index2 == (length part2)) = (s == running)
  | (substr == s1) && (substr == s2) = (check s part1 part2 (index1 + 1) index2 s1) || (check s part1 part2 index1 (index2 + 1) s2)
  | substr == s1 = check s part1 part2 (index1 + 1) index2 s1
  | substr == s2 = check s part1 part2 index1 (index2 + 1) s2
  | otherwise = False
  where s1 = if index1 < (length part1) then (running ++ ([part1!!index1])) else running
        s2 = if index2 < (length part2) then (running ++ ([part2!!index2])) else running
        substr = take (max (length s1) (length s2)) s

isMerge :: String -> String -> String -> Bool
isMerge s part1 part2 = check s part1 part2 0 0 ""

{--
Tests:
isMerge "Bananas from Bahamas" "Bahas" "Bananas from am"
isMerge "" "" ""
isMerge "abcd" "abcd" ""
isMerge "abcd" "" "abcd"
isMerge "abcd" "" "abcde"
--}