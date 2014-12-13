module Combinator (combinations, subsets) where

import Control.Monad

combinations :: [Int] -> [[Int]]
combinations [] = []
combinations [x] = [[x]]
combinations xs = [xs!!i : ys | i <- [0..length xs - 1]
                              , let cutOut = take i xs ++ drop (i+1) xs
                              , ys <- combinations $ cutOut]

subsets :: [Int] -> [[Int]]
subsets xs = filterM (\x -> [True, False]) xs
