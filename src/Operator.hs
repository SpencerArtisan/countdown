module Operator (operateWithMemo, plus, minus, times) where

operateWithMemo :: [(Int,String) -> (Int,String) -> (Int, String)] -> [(Int,String)] -> [(Int,String)]
operateWithMemo _ [] = []
operateWithMemo _ [x] = [x]
operateWithMemo ops (x:y:zs) = 
  concat [operateWithMemo ops (op x y:zs) | op <- ops]

plus :: (Int,String) -> (Int,String) -> (Int, String)
plus = applyOp (+) "+"

minus :: (Int,String) -> (Int,String) -> (Int, String)
minus = applyOp (-) "-"

times :: (Int,String) -> (Int,String) -> (Int, String)
times (x,s)
    | elemAny "+-" s = applyOp (*) "*" (x,"(" ++ s ++ ")")
    | otherwise      = applyOp (*) "*" (x,s)

applyOp :: (Int -> Int -> Int) -> String -> (Int,String) -> (Int,String) -> (Int, String)
applyOp f sym (x,s1) (y,s2) = (f x y, opMemo sym s1 s2)

opMemo :: String -> String -> String -> String
opMemo op s1 s2 = s1 ++ op ++ s2

elemAny :: (Eq a) => [a] -> [a] -> Bool
elemAny xs ys = foldr1 (||) [elem x ys | x <- xs]
