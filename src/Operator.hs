module Operator (operate, multiOperate) where

operate :: (Int -> Int -> Int) -> [Int] -> Int
operate f [] = 0
operate f xs = foldl1 f xs

multiOperate:: [(Int -> Int -> Int)] -> [Int] -> [Int]
multiOperate [] values = []
multiOperate ops values = 
  operate (head ops) values : (multiOperate (tail ops) values)

