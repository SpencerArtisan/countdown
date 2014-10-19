module Combinator (combinations, subsets) where

combinations :: [Int] -> [[Int]]
combinations [] = []
combinations [x] = [[x]]
combinations xs = [(xs!!i) : aa | i <- [0..length xs - 1], aa <- combinations $ cutOut i xs]
    where cutOut i xs = take i xs ++ drop (i+1) xs

subsets :: [Int] -> [[Int]]
subsets [] = []
subsets [x] = [[x]]
subsets (x:xs) = [x] : subsets xs ++ [x:ys | ys <- subsets xs] 
