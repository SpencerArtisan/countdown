module Operator (Solution(..), operateWithMemo, plus, minus, times) where

data Solution = Solution Int String deriving (Show,Eq)

operateWithMemo :: [Solution -> Solution -> Solution] -> [Solution] -> [Solution]
operateWithMemo _ [] = []
operateWithMemo _ [x] = [x]
operateWithMemo ops (x:y:zs) = 
  concat [operateWithMemo ops (op x y:zs) | op <- ops]

plus :: Solution -> Solution -> Solution
plus = applyOp (+) "+"

minus :: Solution -> Solution -> Solution
minus = applyOp (-) "-"

times :: Solution -> Solution -> Solution
times (Solution x s)
    | any (`elem` "+-") s = applyOp (*) "*" $ Solution x $ "(" ++ s ++ ")"
    | otherwise           = applyOp (*) "*" $ Solution x s

applyOp :: (Int -> Int -> Int) -> String -> Solution -> Solution -> Solution
applyOp f sym (Solution x s1) (Solution y s2) = Solution (f x y) (concat [s1,sym,s2])

