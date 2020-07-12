module Lib
    ( someFunc
    ) where

import Worda10n (abbreviate)

someFunc :: IO ()
someFunc = putStrLn (abbreviate "elephant-rides are really fun!")