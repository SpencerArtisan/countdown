module Operator (Solution(..), operateWithMemo, plus, minus, times, divide) where

import Control.Applicative

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

divide :: Solution -> Solution -> Solution
divide (Solution x s1) (Solution y s2)
    | x `mod` y /= 0       = Solution x s1 
    | any (`elem` "+-") s1 = applyOp (div) "/" (Solution x $ "(" ++ s1 ++ ")") (Solution y s2)
    | otherwise            = applyOp (div) "/" (Solution x s1) (Solution y s2)

applyOp :: (Int -> Int -> Int) -> String -> Solution -> Solution -> Solution
applyOp f sym (Solution x s1) (Solution y s2) = Solution (f x y) (concat [s1,sym,s2])

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show,Eq)
type Solution2 = Tree

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node n l r) = Node (f n) (fmap f l) (fmap f r)

{-oerateWithMemo2 :: [Solution] -> [Solution]-}
{-operateWithMemo2 xs = foldl <*> [(*),(+),(-)] xs-}
