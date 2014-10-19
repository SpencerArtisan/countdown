module Countdown (countdown, countdownTarget) where

import Combinator
import Operator

countdown :: [Int] -> [(Int,String)]
countdown [] = []
countdown xs = concat [operateWithMemo ops (map toTuple cm) | ss <- subsets xs, cm <- combinations ss]
    where toTuple c = (c, show c)
          ops = [plus, minus, times]

countdownTarget :: [Int] -> Int -> String
countdownTarget xs t 
    | null calcs = "No answer"
    | otherwise  = snd $ head calcs
    where hitsTarget (v,r) = t == v
          calcs = filter hitsTarget $ countdown xs


