module Combinator (combinations) where

combinations :: [Int] -> [[Int]]
combinations [] = []
combinations [x] = [[x]]
combinations xs = [combinations left ++ combinations right | n <- [1..length xs], left <- take n xs, right <- drop n xs]
