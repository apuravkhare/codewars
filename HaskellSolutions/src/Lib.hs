module Lib
    ( someFunc
    ) where

import Worda10n (abbreviate)
import MergedStringChecker (isMerge)

someFunc :: IO ()
someFunc = do { putStrLn (show (isMerge "Bananas from Bahamas" "Bahas" "Bananas from am"))
              ; putStrLn (show (isMerge "" "" "")) 
              ; putStrLn (show (isMerge "abcd" "abcd" ""))
              ; putStrLn (show (isMerge "abcd" "" "abcd"))
              ; putStrLn (show (isMerge "abcd" "" "abcde")) }