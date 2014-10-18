module Combinator (combinations) where

combinations :: [Int] -> [[Int]]
combinations [] = []
combinations [x] = [[x]]
combinations xs = [(xs!!i) : aa | i <- [0..length xs - 1], aa <- combinations $ (take i xs ++ drop (i+1) xs)]
