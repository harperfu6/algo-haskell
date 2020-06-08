-- HashTableを使ったユースケース
module Main where

import Data.HashTable


regword :: (HashTable String Int) -> [String] -> IO()
regword _ [] = return ()
regword h (w:ws) = 