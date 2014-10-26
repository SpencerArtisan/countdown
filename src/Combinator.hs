module Combinator (combinations, subsets) where

combinations :: [Int] -> [[Int]]
combinations [] = []
combinations [x] = [[x]]
combinations xs = [xs!!i : ys | i <- [0..length xs - 1]
                              , let cutOut = take i xs ++ drop (i+1) xs
                              , ys <- combinations $ cutOut]

subsets :: [Int] -> [[Int]]
subsets [] = []
subsets [x] = [[x]]
subsets (x:xs) = [x] : subsets xs ++ [x:ys | ys <- subsets xs] 
