module Operator (operate, multiOperate, operateWithMemo, plus, minus, times) where

operate :: (Int -> Int -> Int) -> [Int] -> Int
operate f [] = 0
operate f xs = foldl1 f xs

multiOperate:: [(Int -> Int -> Int)] -> [Int] -> [Int]
multiOperate ops [x] = [x]
multiOperate ops (x:y:zs) = 
  concat [multiOperate ops ((operate op [x,y]):zs) | op <- ops]

operateWithMemo :: [(Int,String) -> (Int,String) -> (Int, String)] -> [(Int,String)] -> [(Int,String)]
operateWithMemo ops [] = []
operateWithMemo ops [x] = [x]
operateWithMemo ops (x:y:zs) = 
  concat [operateWithMemo ops ((op x y):zs) | op <- ops]

plus :: (Int,String) -> (Int,String) -> (Int, String)
plus (x,s1) (y,s2) = (x+y,s1 ++ "+" ++ s2)

minus :: (Int,String) -> (Int,String) -> (Int, String)
minus (x,s1) (y,s2) = (x-y,s1 ++ "-" ++ s2)

times :: (Int,String) -> (Int,String) -> (Int, String)
times (x,s1) (y,s2) 
    | elem '-' s1 || elem '+' s1 = (x*y,"(" ++ s1 ++ ")*" ++ s2)
    | otherwise = (x*y,s1 ++ "*" ++ s2)

