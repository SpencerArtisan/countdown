module Operator (operate, multiOperate) where

operate :: (Int -> Int -> Int) -> [Int] -> Int
operate f [] = 0
operate f xs = foldl1 f xs

multiOperate:: [(Int -> Int -> Int)] -> [Int] -> [Int]
multiOperate ops [x] = [x]
multiOperate ops (x:y:zs) = 
  concat [multiOperate ops ((operate op [x,y]):zs) | op <- ops]

